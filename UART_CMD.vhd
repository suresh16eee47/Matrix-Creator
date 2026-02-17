library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- ============================================================
-- everloop_led : SK6812RGBW (GRBW) one-shot streamer + UART control + UART ACK
-- Clock: 50 MHz
-- UART: 115200 8N1 (generic)
-- LEDs: 31 (generic)
--
-- Key behavior:
--  - led_dout stays LOW (idle) when there is no change.
--  - After a VALID UART command updates a LED color, a single full SK6812 frame
--    is transmitted, then line returns LOW again (no continuous refresh).
--  - UART ACK is sent for every parsed command (OK or error).
--
-- RX command (8 bytes):
--   AA  idx  G  R  B  W  chk  55
--   chk = XOR(bytes 0..5)
--
-- TX response (6 bytes):
--   CC  idx  status  echo_chk  resp_chk  33
--   resp_chk = XOR(CC, idx, status, echo_chk)
--   status:
--     00 OK
--     01 BAD_CHECKSUM
--     02 BAD_END_BYTE
--     03 BAD_INDEX
--
-- reset_n: active-low
-- ============================================================

entity everloop_led is
  generic (
    CLK_HZ : natural := 50_000_000;
    BAUD   : natural := 115200;
    N_LEDS : natural := 31
  );
  port (
    clk50    : in  std_logic;
    reset_n  : in  std_logic;  -- active-low
    uart_rx  : in  std_logic;
    uart_tx  : out std_logic;
    led_dout : out std_logic
  );
end entity;

architecture rtl of everloop_led is

  ----------------------------------------------------------------------------
  -- LED memory: GRBW packed as [31:24]=G [23:16]=R [15:8]=B [7:0]=W
  ----------------------------------------------------------------------------
  type led_mem_t is array (0 to N_LEDS-1) of std_logic_vector(31 downto 0);
  signal led_mem : led_mem_t := (others => (others => '0'));

  ----------------------------------------------------------------------------
  -- UART constants
  ----------------------------------------------------------------------------
  constant BAUD_DIV : natural := CLK_HZ / BAUD; -- e.g. 434 for 50MHz/115200

  ----------------------------------------------------------------------------
  -- UART RX (8N1) signals
  ----------------------------------------------------------------------------
  type urx_state_t is (URX_IDLE, URX_START, URX_DATA, URX_STOP);
  signal urx_state  : urx_state_t := URX_IDLE;
  signal urx_divcnt : natural range 0 to BAUD_DIV := 0;
  signal urx_bitcnt : integer range 0 to 7 := 0;
  signal urx_shift  : std_logic_vector(7 downto 0) := (others => '0');
  signal rx_byte    : std_logic_vector(7 downto 0) := (others => '0');
  signal rx_strobe  : std_logic := '0';

  -- RX synchronizer
  signal rx_ff1, rx_ff2 : std_logic := '1';
  signal rx_sync        : std_logic := '1';

  ----------------------------------------------------------------------------
  -- UART TX (8N1) byte sender
  ----------------------------------------------------------------------------
  type utx_state_t is (UTX_IDLE, UTX_START, UTX_DATA, UTX_STOP);
  signal utx_state  : utx_state_t := UTX_IDLE;
  signal utx_divcnt : natural range 0 to BAUD_DIV := 0;
  signal utx_bitcnt : integer range 0 to 7 := 0;
  signal utx_shift  : std_logic_vector(7 downto 0) := (others => '0');
  signal utx_line   : std_logic := '1';

  signal tx_busy  : std_logic := '0';
  signal tx_start : std_logic := '0';  -- pulse
  signal tx_byte  : std_logic_vector(7 downto 0) := (others => '0');

  ----------------------------------------------------------------------------
  -- ACK sender
  ----------------------------------------------------------------------------
  type ack_state_t is (A_IDLE, A_B0, A_B1, A_B2, A_B3, A_B4, A_B5);
  signal ack_state : ack_state_t := A_IDLE;

  signal ack_idx      : std_logic_vector(7 downto 0) := (others => '0');
  signal ack_status   : std_logic_vector(7 downto 0) := (others => '0');
  signal ack_echo_chk : std_logic_vector(7 downto 0) := (others => '0');
  signal ack_resp_chk : std_logic_vector(7 downto 0) := (others => '0');

  -- ack_pending single-driver using set/clear pulses
  signal ack_pending : std_logic := '0';
  signal ack_set     : std_logic := '0';
  signal ack_clr     : std_logic := '0';

  ----------------------------------------------------------------------------
  -- Dirty flag (send SK6812 only on change)
  ----------------------------------------------------------------------------
  signal dirty     : std_logic := '0';
  signal dirty_set : std_logic := '0';
  signal dirty_clr : std_logic := '0';

  ----------------------------------------------------------------------------
  -- Command parser: AA idx G R B W chk 55
  ----------------------------------------------------------------------------
  type parse_state_t is (P_WAIT_AA, P_IDX, P_G, P_R, P_B, P_W, P_CHK, P_WAIT_55);
  signal pstate : parse_state_t := P_WAIT_AA;

  signal tmp_idx_nat : natural range 0 to N_LEDS-1 := 0;
  signal tmp_idx_b   : std_logic_vector(7 downto 0) := (others => '0');
  signal tmp_g, tmp_r, tmp_b, tmp_w : std_logic_vector(7 downto 0) := (others => '0');
  signal tmp_chk  : std_logic_vector(7 downto 0) := (others => '0');
  signal calc_xor : std_logic_vector(7 downto 0) := (others => '0');

  ----------------------------------------------------------------------------
  -- SK6812RGBW timing @50MHz
  ----------------------------------------------------------------------------
  constant TBIT : natural := 63;    -- ~1.26us
  constant T0H  : natural := 15;    -- ~0.30us
  constant T1H  : natural := 30;    -- ~0.60us
  constant TRES : natural := 5000;  -- 100us low reset

  type led_state_t is (S_IDLE, S_LOAD, S_SEND_BIT, S_NEXT_BIT, S_NEXT_LED, S_LATCH_LOW);
  signal lstate : led_state_t := S_IDLE;

  signal led_cyc_cnt : natural range 0 to 10000 := 0;
  signal led_idx     : natural range 0 to N_LEDS-1 := 0;
  signal bit_idx     : integer range 0 to 31 := 31;
  signal pix32       : std_logic_vector(31 downto 0) := (others => '0');
  signal dout        : std_logic := '0';

begin
  uart_tx  <= utx_line;
  led_dout <= dout;

  ----------------------------------------------------------------------------
  -- RX synchronizer
  ----------------------------------------------------------------------------
  process(clk50)
  begin
    if rising_edge(clk50) then
      rx_ff1  <= uart_rx;
      rx_ff2  <= rx_ff1;
      rx_sync <= rx_ff2;
    end if;
  end process;

  ----------------------------------------------------------------------------
  -- UART RX (8N1, mid-bit sampling)
  ----------------------------------------------------------------------------
  process(clk50)
  begin
    if rising_edge(clk50) then
      rx_strobe <= '0';

      if reset_n = '0' then
        urx_state  <= URX_IDLE;
        urx_divcnt <= 0;
        urx_bitcnt <= 0;
        urx_shift  <= (others => '0');
      else
        case urx_state is
          when URX_IDLE =>
            if rx_sync = '0' then
              urx_state  <= URX_START;
              urx_divcnt <= 0;
            end if;

          when URX_START =>
            if urx_divcnt = (BAUD_DIV/2) then
              urx_divcnt <= 0;
              if rx_sync = '0' then
                urx_state  <= URX_DATA;
                urx_bitcnt <= 0;
              else
                urx_state <= URX_IDLE;
              end if;
            else
              urx_divcnt <= urx_divcnt + 1;
            end if;

          when URX_DATA =>
            if urx_divcnt = BAUD_DIV-1 then
              urx_divcnt <= 0;
              urx_shift(urx_bitcnt) <= rx_sync; -- LSB first
              if urx_bitcnt = 7 then
                urx_state <= URX_STOP;
              else
                urx_bitcnt <= urx_bitcnt + 1;
              end if;
            else
              urx_divcnt <= urx_divcnt + 1;
            end if;

          when URX_STOP =>
            if urx_divcnt = BAUD_DIV-1 then
              urx_divcnt <= 0;
              if rx_sync = '1' then
                rx_byte   <= urx_shift;
                rx_strobe <= '1';
              end if;
              urx_state <= URX_IDLE;
            else
              urx_divcnt <= urx_divcnt + 1;
            end if;
        end case;
      end if;
    end if;
  end process;

  ----------------------------------------------------------------------------
  -- UART TX byte sender (8N1). Accept tx_start only when idle.
  ----------------------------------------------------------------------------
  process(clk50)
  begin
    if rising_edge(clk50) then
      if reset_n = '0' then
        utx_state  <= UTX_IDLE;
        utx_divcnt <= 0;
        utx_bitcnt <= 0;
        utx_shift  <= (others => '0');
        utx_line   <= '1';
        tx_busy    <= '0';
      else
        case utx_state is
          when UTX_IDLE =>
            utx_line <= '1';
            tx_busy  <= '0';
            if tx_start = '1' then
              utx_shift  <= tx_byte;
              utx_state  <= UTX_START;
              utx_divcnt <= 0;
              tx_busy    <= '1';
            end if;

          when UTX_START =>
            utx_line <= '0';
            if utx_divcnt = BAUD_DIV-1 then
              utx_divcnt <= 0;
              utx_bitcnt <= 0;
              utx_state  <= UTX_DATA;
            else
              utx_divcnt <= utx_divcnt + 1;
            end if;

          when UTX_DATA =>
            utx_line <= utx_shift(utx_bitcnt);
            if utx_divcnt = BAUD_DIV-1 then
              utx_divcnt <= 0;
              if utx_bitcnt = 7 then
                utx_state <= UTX_STOP;
              else
                utx_bitcnt <= utx_bitcnt + 1;
              end if;
            else
              utx_divcnt <= utx_divcnt + 1;
            end if;

          when UTX_STOP =>
            utx_line <= '1'; -- full stop-bit time
            if utx_divcnt = BAUD_DIV-1 then
              utx_divcnt <= 0;
              utx_state  <= UTX_IDLE;
            else
              utx_divcnt <= utx_divcnt + 1;
            end if;
        end case;
      end if;
    end if;
  end process;

  ----------------------------------------------------------------------------
  -- ack_pending single-driver latch (set/clear pulses)
  ----------------------------------------------------------------------------
  process(clk50)
  begin
    if rising_edge(clk50) then
      if reset_n = '0' then
        ack_pending <= '0';
      else
        if ack_set = '1' then
          ack_pending <= '1';
        elsif ack_clr = '1' then
          ack_pending <= '0';
        end if;
      end if;
    end if;
  end process;

  ----------------------------------------------------------------------------
  -- dirty single-driver latch (set/clear pulses)
  ----------------------------------------------------------------------------
  process(clk50)
  begin
    if rising_edge(clk50) then
      if reset_n = '0' then
        dirty <= '0';
      else
        if dirty_set = '1' then
          dirty <= '1';
        elsif dirty_clr = '1' then
          dirty <= '0';
        end if;
      end if;
    end if;
  end process;

  ----------------------------------------------------------------------------
  -- Command parser + LED write + prepare ACK + pulses ack_set and dirty_set
  ----------------------------------------------------------------------------
  process(clk50)
    variable b     : std_logic_vector(7 downto 0);
    variable idx_u : unsigned(7 downto 0);
    variable st    : std_logic_vector(7 downto 0);
  begin
    if rising_edge(clk50) then
      if reset_n = '0' then
        pstate    <= P_WAIT_AA;
        calc_xor  <= (others => '0');
        tmp_chk   <= (others => '0');
        ack_set   <= '0';
        dirty_set <= '0';
      else
        ack_set   <= '0';  -- 1-cycle pulse
        dirty_set <= '0';  -- 1-cycle pulse

        if rx_strobe = '1' then
          b := rx_byte;

          case pstate is
            when P_WAIT_AA =>
              if b = x"AA" then
                calc_xor <= x"AA";
                pstate   <= P_IDX;
              end if;

            when P_IDX =>
              idx_u := unsigned(b);
              if to_integer(idx_u) < integer(N_LEDS) then
                tmp_idx_nat <= to_integer(idx_u);
                tmp_idx_b   <= b;
                calc_xor    <= calc_xor xor b;
                pstate      <= P_G;
              else
                -- BAD_INDEX => immediate NACK
                ack_idx      <= b;
                ack_echo_chk <= x"00";
                ack_status   <= x"03";
                ack_resp_chk <= x"CC" xor b xor x"03" xor x"00";
                ack_set      <= '1';
                pstate       <= P_WAIT_AA;
              end if;

            when P_G =>
              tmp_g    <= b; calc_xor <= calc_xor xor b; pstate <= P_R;

            when P_R =>
              tmp_r    <= b; calc_xor <= calc_xor xor b; pstate <= P_B;

            when P_B =>
              tmp_b    <= b; calc_xor <= calc_xor xor b; pstate <= P_W;

            when P_W =>
              tmp_w    <= b; calc_xor <= calc_xor xor b; pstate <= P_CHK;

            when P_CHK =>
              tmp_chk <= b;
              pstate  <= P_WAIT_55;

            when P_WAIT_55 =>
              -- Prepare ACK fields
              ack_idx      <= tmp_idx_b;
              ack_echo_chk <= tmp_chk;

              if b /= x"55" then
                st := x"02"; -- BAD_END_BYTE
              elsif tmp_chk /= calc_xor then
                st := x"01"; -- BAD_CHECKSUM
              else
                st := x"00"; -- OK: apply update
                led_mem(tmp_idx_nat) <= tmp_g & tmp_r & tmp_b & tmp_w; -- GRBW
                dirty_set <= '1'; -- trigger one-shot LED refresh
              end if;

              ack_status   <= st;
              ack_resp_chk <= x"CC" xor tmp_idx_b xor st xor tmp_chk;
              ack_set      <= '1';

              pstate <= P_WAIT_AA;
          end case;
        end if;
      end if;
    end if;
  end process;

  ----------------------------------------------------------------------------
  -- ACK transmitter FSM (6 bytes) + clear ack_pending at end
  ----------------------------------------------------------------------------
  process(clk50)
  begin
    if rising_edge(clk50) then
      if reset_n = '0' then
        ack_state <= A_IDLE;
        tx_start  <= '0';
        tx_byte   <= (others => '0');
        ack_clr   <= '0';
      else
        tx_start <= '0';
        ack_clr  <= '0';

        case ack_state is
          when A_IDLE =>
            if ack_pending = '1' then
              ack_state <= A_B0;
            end if;

          when A_B0 =>
            if tx_busy = '0' then
              tx_byte   <= x"CC";
              tx_start  <= '1';
              ack_state <= A_B1;
            end if;

          when A_B1 =>
            if tx_busy = '0' then
              tx_byte   <= ack_idx;
              tx_start  <= '1';
              ack_state <= A_B2;
            end if;

          when A_B2 =>
            if tx_busy = '0' then
              tx_byte   <= ack_status;
              tx_start  <= '1';
              ack_state <= A_B3;
            end if;

          when A_B3 =>
            if tx_busy = '0' then
              tx_byte   <= ack_echo_chk;
              tx_start  <= '1';
              ack_state <= A_B4;
            end if;

          when A_B4 =>
            if tx_busy = '0' then
              tx_byte   <= ack_resp_chk;
              tx_start  <= '1';
              ack_state <= A_B5;
            end if;

          when A_B5 =>
            if tx_busy = '0' then
              tx_byte   <= x"33";
              tx_start  <= '1';
              ack_state <= A_IDLE;
              ack_clr   <= '1';
            end if;
        end case;
      end if;
    end if;
  end process;

  ----------------------------------------------------------------------------
  -- SK6812 one-shot streamer:
  --  - dout LOW in S_IDLE
  --  - when dirty=1, send full frame then hold LOW for TRES then dirty_clr
  ----------------------------------------------------------------------------
  process(clk50)
    variable this_bit : std_logic;
    variable t_high   : natural;
  begin
    if rising_edge(clk50) then
      if reset_n = '0' then
        lstate      <= S_IDLE;
        led_cyc_cnt <= 0;
        led_idx     <= 0;
        bit_idx     <= 31;
        pix32       <= (others => '0');
        dout        <= '0';
        dirty_clr   <= '0';
      else
        dirty_clr <= '0'; -- pulse

        case lstate is
          when S_IDLE =>
            dout        <= '0';
            led_cyc_cnt <= 0;
            if dirty = '1' then
              led_idx <= 0;
              lstate  <= S_LOAD;
            end if;

          when S_LOAD =>
            pix32       <= led_mem(led_idx);
            bit_idx     <= 31;
            led_cyc_cnt <= 0;
            lstate      <= S_SEND_BIT;

          when S_SEND_BIT =>
            this_bit := pix32(bit_idx);
            if this_bit = '1' then t_high := T1H; else t_high := T0H; end if;

            if led_cyc_cnt < t_high then
              dout        <= '1';
              led_cyc_cnt <= led_cyc_cnt + 1;
            elsif led_cyc_cnt < TBIT then
              dout        <= '0';
              led_cyc_cnt <= led_cyc_cnt + 1;
            else
              dout        <= '0';
              led_cyc_cnt <= 0;
              lstate      <= S_NEXT_BIT;
            end if;

          when S_NEXT_BIT =>
            if bit_idx = 0 then
              lstate <= S_NEXT_LED;
            else
              bit_idx <= bit_idx - 1;
              lstate  <= S_SEND_BIT;
            end if;

          when S_NEXT_LED =>
            if led_idx = N_LEDS - 1 then
              led_cyc_cnt <= 0;
              lstate      <= S_LATCH_LOW;
            else
              led_idx <= led_idx + 1;
              lstate  <= S_LOAD;
            end if;

          when S_LATCH_LOW =>
            dout <= '0';
            if led_cyc_cnt >= TRES then
              led_cyc_cnt <= 0;
              lstate      <= S_IDLE;
              dirty_clr   <= '1'; -- clear dirty after one-shot completes
            else
              led_cyc_cnt <= led_cyc_cnt + 1;
            end if;
        end case;
      end if;
    end if;
  end process;

end architecture;