library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_read_default_reg_clkmon is
end entity;

architecture sim of tb_read_default_reg_clkmon is
  constant CLK_HZ : integer := 50_000_000;
  constant BAUD   : integer := 115200;

  constant CLK_PERIOD : time := 20 ns;
  constant BAUD_DIV   : integer := CLK_HZ / BAUD;  -- 434 clocks/bit (matches DUT)

  signal clk50    : std_logic := '0';
  signal reset_n  : std_logic := '0';
  signal uart_rx  : std_logic := '1';
  signal uart_tx  : std_logic;
  signal led_dout : std_logic;

  -- ------------ HEX helpers (VHDL-93 compatible) ------------
  function nibble_to_hex(n : std_logic_vector(3 downto 0)) return character is
    variable u : integer := to_integer(unsigned(n));
  begin
    if u < 10 then
      return character'val(character'pos('0') + u);
    else
      return character'val(character'pos('A') + (u - 10));
    end if;
  end function;

  function byte_to_hex(b : std_logic_vector(7 downto 0)) return string is
    variable s : string(1 to 2);
  begin
    s(1) := nibble_to_hex(b(7 downto 4));
    s(2) := nibble_to_hex(b(3 downto 0));
    return s;
  end function;

  -- ------------ TX (Pi->FPGA) byte sender using time ------------
  -- This is OK because it doesn't need to decode; it only drives uart_rx.
  constant BIT_TIME : time := BAUD_DIV * CLK_PERIOD;

  procedure uart_send_byte(signal line : out std_logic; b : std_logic_vector(7 downto 0)) is
  begin
    line <= '0'; wait for BIT_TIME;         -- start
    for i in 0 to 7 loop
      line <= b(i); wait for BIT_TIME;      -- LSB first
    end loop;
    line <= '1'; wait for BIT_TIME;         -- stop
  end procedure;

  -- ------------ UART monitor outputs ------------
  type byte_array_t is array (0 to 7) of std_logic_vector(7 downto 0);
  signal rx_bytes : byte_array_t := (others => (others => '0'));
  signal rx_count : integer range 0 to 8 := 0;
  signal got_packet : std_logic := '0';

begin
  -- clock generation
  clk50 <= not clk50 after CLK_PERIOD/2;

  -- DUT
  dut: entity work.everloop_top_ext
    generic map (
      CLK_HZ      => CLK_HZ,
      BAUD        => BAUD,
      N_LEDS      => 31,
      MAX_PAYLOAD => 32,
      MAX_RESP    => 32
    )
    port map (
      clk50    => clk50,
      reset_n  => reset_n,
      uart_rx  => uart_rx,
      uart_tx  => uart_tx,
      led_dout => led_dout
    );

  --------------------------------------------------------------------
  -- UART TX MONITOR (cycle-counting, no wait-for timing drift)
  --------------------------------------------------------------------
  monitor: process(clk50)
    type mstate_t is (M_IDLE, M_START, M_DATA, M_STOP);
    variable st : mstate_t := M_IDLE;

    variable prev_tx : std_logic := '1';
    variable cnt     : integer := 0;
    variable bit_idx : integer := 0;
    variable sh      : std_logic_vector(7 downto 0) := (others => '0');

    -- sample points
    constant HALF_BIT : integer := BAUD_DIV/2;      -- 217
    constant BIT_END  : integer := BAUD_DIV-1;      -- 433
  begin
    if rising_edge(clk50) then
      got_packet <= '0';

      if reset_n = '0' then
        st       := M_IDLE;
        prev_tx  := '1';
        cnt      := 0;
        bit_idx  := 0;
        sh       := (others => '0');
        rx_count <= 0;
      else
        case st is
          when M_IDLE =>
            cnt := 0;
            -- detect falling edge to start bit
            if (prev_tx = '1') and (uart_tx = '0') then
              st  := M_START;
              cnt := 0;
            end if;

          when M_START =>
            -- validate start bit at mid start
            if cnt = HALF_BIT then
              if uart_tx /= '0' then
                -- false start, go back
                st := M_IDLE;
              end if;
            end if;

            if cnt = BIT_END then
              st      := M_DATA;
              cnt     := 0;
              bit_idx := 0;
            else
              cnt := cnt + 1;
            end if;

          when M_DATA =>
            -- sample each data bit at mid-bit
            if cnt = HALF_BIT then
              sh(bit_idx) := uart_tx; -- LSB first
            end if;

            if cnt = BIT_END then
              cnt := 0;
              if bit_idx = 7 then
                st := M_STOP;
              else
                bit_idx := bit_idx + 1;
              end if;
            else
              cnt := cnt + 1;
            end if;

          when M_STOP =>
            -- sample stop bit at mid stop
            if cnt = HALF_BIT then
              -- If this fails here, TX truly violated stop, not TB drift.
              assert uart_tx = '1'
                report "STOP BIT LOW (true violation at mid-stop)" severity warning;
            end if;

            if cnt = BIT_END then
              -- push byte
              if rx_count < 8 then
                rx_bytes(rx_count) <= sh;
                rx_count <= rx_count + 1;
                if rx_count = 7 then
                  got_packet <= '1';
                end if;
              end if;
              st  := M_IDLE;
              cnt := 0;
            else
              cnt := cnt + 1;
            end if;
        end case;

        prev_tx := uart_tx;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- STIMULUS + CHECKER
  --------------------------------------------------------------------
  stim: process
    variable b0,b1,b2,b3,b4,b5,b6,b7 : std_logic_vector(7 downto 0);
  begin
    uart_rx <= '1';

    reset_n <= '0';
    wait for