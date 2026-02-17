library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- ============================================================
-- Extensible Everloop Top
-- Protocol:
--   RX: AA CMD LEN PAYLOAD[LEN] CHK 55     CHK = XOR(all previous bytes)
--   TX: CC CMD STATUS LEN DATA[LEN] CHK 33 CHK = XOR(all previous bytes)
--
-- Implemented commands (in cmd_dispatch):
--   0x10 SET_LED  (LEN=5) payload: idx,G,R,B,W
--   0x11 SET_ALL  (LEN=4) payload: G,R,B,W
--   0x20 MAG_READ (stub)  (LEN=0) response: 6 bytes zeros
--
-- SK6812RGBW:
--   one-shot update only on change (dirty flag)
-- ============================================================

entity everloop_top_ext is
  generic (
    CLK_HZ      : natural := 50_000_000;
    BAUD        : natural := 115200;
    N_LEDS      : natural := 31;
    MAX_PAYLOAD : natural := 32;
    MAX_RESP    : natural := 32
  );
  port (
    clk50    : in  std_logic;
    reset_n  : in  std_logic; -- active-low
    uart_rx  : in  std_logic;
    uart_tx  : out std_logic;
    led_dout : out std_logic
  );
end entity;

architecture rtl of everloop_top_ext is

  -- LED memory: GRBW packed [31:24]=G [23:16]=R [15:8]=B [7:0]=W
  type led_mem_t is array (0 to N_LEDS-1) of std_logic_vector(31 downto 0);
  signal led_mem : led_mem_t := (others => (others => '0'));

  -- UART byte stream
  signal rx_byte   : std_logic_vector(7 downto 0) := (others => '0');
  signal rx_strobe : std_logic := '0';

  signal tx_start  : std_logic := '0';
  signal tx_byte   : std_logic_vector(7 downto 0) := (others => '0');
  signal tx_busy   : std_logic := '0';

  -- Frame decoder outputs
  signal frame_valid : std_logic := '0';
  signal f_cmd       : std_logic_vector(7 downto 0) := (others=>'0');
  signal f_len       : std_logic_vector(7 downto 0) := (others=>'0');
  signal f_payload   : std_logic_vector(MAX_PAYLOAD*8-1 downto 0) := (others=>'0');

  signal err_valid   : std_logic := '0';
  signal err_cmd     : std_logic_vector(7 downto 0) := (others=>'0');
  signal err_code    : std_logic_vector(7 downto 0) := (others=>'0');

  -- Command dispatch outputs (LED write + response request)
  signal led_wr_pulse  : std_logic := '0';
  signal led_all_pulse : std_logic := '0';
  signal led_idx_nat   : natural range 0 to N_LEDS-1 := 0;
  signal led_pix_grbw  : std_logic_vector(31 downto 0) := (others=>'0');

  signal resp_req      : std_logic := '0';
  signal resp_cmd      : std_logic_vector(7 downto 0) := (others=>'0');
  signal resp_status   : std_logic_vector(7 downto 0) := (others=>'0');
  signal resp_len      : std_logic_vector(7 downto 0) := (others=>'0');
  signal resp_data     : std_logic_vector(MAX_RESP*8-1 downto 0) := (others=>'0');

  -- Response TX busy (frame-level)
  signal resp_busy : std_logic := '0';

  -- Dirty latch for one-shot SK6812 send
  signal dirty     : std_logic := '0';
  signal dirty_set : std_logic := '0';
  signal dirty_clr : std_logic := '0';

  -- SK6812 timing @50MHz
  constant TBIT : natural := 63;    -- ~1.26us
  constant T0H  : natural := 15;    -- ~0.30us
  constant T1H  : natural := 30;    -- ~0.60us
  constant TRES : natural := 5000;  -- 100us low reset/latch

  type led_state_t is (S_IDLE, S_LOAD, S_SEND_BIT, S_NEXT_BIT, S_NEXT_LED, S_LATCH_LOW, S_CLEAR);
  signal lstate : led_state_t := S_IDLE;

  signal led_cyc_cnt : natural range 0 to 10000 := 0;
  signal s_led_idx   : natural range 0 to N_LEDS-1 := 0;
  signal bit_idx     : integer range 0 to 31 := 31;
  signal pix32       : std_logic_vector(31 downto 0) := (others => '0');
  signal dout        : std_logic := '0';

begin
  led_dout <= dout;

  -- UART RX
  u_rx: entity work.uart_rx_8n1
    generic map (CLK_HZ => CLK_HZ, BAUD => BAUD)
    port map (
      clk        => clk50,
      reset_n    => reset_n,
      rx         => uart_rx,
      data_out   => rx_byte,
      data_valid => rx_strobe
    );

  -- UART TX (byte-level)
  u_tx: entity work.uart_tx_8n1
    generic map (CLK_HZ => CLK_HZ, BAUD => BAUD)
    port map (
      clk     => clk50,
      reset_n => reset_n,
      start   => tx_start,
      data_in => tx_byte,
      busy    => tx_busy,
      tx      => uart_tx
    );

  -- Frame decoder
  u_frx: entity work.uart_frame_rx
    generic map (MAX_PAYLOAD => MAX_PAYLOAD)
    port map (
      clk         => clk50,
      reset_n     => reset_n,
      rx_byte     => rx_byte,
      rx_strobe   => rx_strobe,
      frame_valid => frame_valid,
      cmd         => f_cmd,
      len         => f_len,
      payload     => f_payload,
      err_valid   => err_valid,
      err_cmd     => err_cmd,
      err_code    => err_code
    );

  -- Command dispatcher (add future peripherals here)
  u_cmd: entity work.cmd_dispatch
    generic map (
      N_LEDS      => N_LEDS,
      MAX_PAYLOAD => MAX_PAYLOAD,
      MAX_RESP    => MAX_RESP
    )
    port map (
      clk          => clk50,
      reset_n      => reset_n,
      frame_valid  => frame_valid,
      cmd          => f_cmd,
      len          => f_len,
      payload      => f_payload,
      err_valid    => err_valid,
      err_cmd      => err_cmd,
      err_code     => err_code,
      led_wr_pulse => led_wr_pulse,
      led_all_pulse=> led_all_pulse,
      led_idx_nat  => led_idx_nat,
      led_pix_grbw => led_pix_grbw,
      resp_req     => resp_req,
      resp_cmd     => resp_cmd,
      resp_status  => resp_status,
      resp_len     => resp_len,
      resp_data    => resp_data
    );

  -- Response transmitter (frame-level)
  u_rtx: entity work.resp_tx
    generic map (
      CLK_HZ   => CLK_HZ,
      BAUD     => BAUD,
      MAX_RESP => MAX_RESP
    )
    port map (
      clk      => clk50,
      reset_n  => reset_n,
      req      => resp_req,
      cmd      => resp_cmd,
      status   => resp_status,
      len      => resp_len,
      data     => resp_data,
      tx_busy  => tx_busy,
      tx_start => tx_start,
      tx_byte  => tx_byte,
      busy     => resp_busy
    );

  -- Apply LED writes and set dirty
  process(clk50)
    variable i : integer;
  begin
    if rising_edge(clk50) then
      if reset_n = '0' then
        dirty_set <= '0';
        -- leave led_mem initialized to 0 (all off)
      else
        dirty_set <= '0';

        if led_wr_pulse = '1' then
          led_mem(led_idx_nat) <= led_pix_grbw;
          dirty_set <= '1';
        elsif led_all_pulse = '1' then
          for i in 0 to integer(N_LEDS)-1 loop
            led_mem(i) <= led_pix_grbw;
          end loop;
          dirty_set <= '1';
        end if;
      end if;
    end if;
  end process;

  -- Dirty latch (clear has priority)
  process(clk50)
  begin
    if rising_edge(clk50) then
      if reset_n = '0' then
        dirty <= '0';
      else
        if dirty_clr = '1' then
          dirty <= '0';
        elsif dirty_set = '1' then
          dirty <= '1';
        end if;
      end if;
    end if;
  end process;

  -- SK6812 one-shot streamer
  process(clk50)
    variable this_bit : std_logic;
    variable t_high   : natural;
  begin
    if rising_edge(clk50) then
      if reset_n = '0' then
        lstate      <= S_IDLE;
        led_cyc_cnt <= 0;
        s_led_idx   <= 0;
        bit_idx     <= 31;
        pix32       <= (others => '0');
        dout        <= '0';
        dirty_clr   <= '0';
      else
        dirty_clr <= '0';

        case lstate is
          when S_IDLE =>
            dout        <= '0';
            led_cyc_cnt <= 0;
            if dirty = '1' then
              s_led_idx <= 0;
              lstate    <= S_LOAD;
            end if;

          when S_LOAD =>
            pix32       <= led_mem(s_led_idx);
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
            if s_led_idx = N_LEDS - 1 then
              led_cyc_cnt <= 0;
              lstate      <= S_LATCH_LOW;
            else
              s_led_idx <= s_led_idx + 1;
              lstate    <= S_LOAD;
            end if;

          when S_LATCH_LOW =>
            dout <= '0';
            if led_cyc_cnt >= TRES then
              led_cyc_cnt <= 0;
              dirty_clr   <= '1';    -- request clear
              lstate      <= S_CLEAR; -- wait 1 clk so dirty clears before re-check
            else
              led_cyc_cnt <= led_cyc_cnt + 1;
            end if;

          when S_CLEAR =>
            dout   <= '0';
            lstate <= S_IDLE;

        end case;
      end if;
    end if;
  end process;

end architecture;