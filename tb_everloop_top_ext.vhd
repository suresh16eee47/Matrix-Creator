library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_read_default_reg is
end entity;

architecture sim of tb_read_default_reg is
  -- Match your DUT generics
  constant CLK_HZ : integer := 50_000_000;
  constant BAUD   : integer := 115200;

  constant CLK_PERIOD : time := 20 ns;
  constant BAUD_DIV   : integer := CLK_HZ / BAUD;          -- 434
  constant BIT_TIME   : time := BAUD_DIV * CLK_PERIOD;     -- 8.68 us approx

  signal clk50    : std_logic := '0';
  signal reset_n  : std_logic := '0';
  signal uart_rx  : std_logic := '1';  -- idle high
  signal uart_tx  : std_logic;
  signal led_dout : std_logic;

  -- -------- HEX helper (works in VHDL-93/2002/2008) --------
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

  -- -------- UART helpers --------
  procedure uart_send_byte(signal line : out std_logic; b : std_logic_vector(7 downto 0)) is
  begin
    -- start bit
    line <= '0';
    wait for BIT_TIME;

    -- data bits LSB first
    for i in 0 to 7 loop
      line <= b(i);
      wait for BIT_TIME;
    end loop;

    -- stop bit
    line <= '1';
    wait for BIT_TIME;
  end procedure;

  procedure uart_recv_byte(signal line : in std_logic; variable b : out std_logic_vector(7 downto 0)) is
  begin
    -- wait for start bit falling edge
    wait until (line'event and line = '0');

    -- move to center of bit0 (1.5 bit times from start edge)
    wait for BIT_TIME + (BIT_TIME/2);

    -- sample 8 data bits (LSB first)
    for i in 0 to 7 loop
      b(i) := line;
      wait for BIT_TIME;
    end loop;

    -- sample stop bit center (optional check)
    if line /= '1' then
      report "WARNING: stop bit not high" severity warning;
    end if;

    wait for BIT_TIME/2;
  end procedure;

begin
  -- clock
  clk50 <= not clk50 after CLK_PERIOD/2;

  -- DUT (your top module)
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

  stim: process
    variable b0,b1,b2,b3,b4,b5,b6,b7 : std_logic_vector(7 downto 0);
  begin
    -- reset
    uart_rx <= '1';
    reset_n <= '0';
    wait for 10 us;
    reset_n <= '1';
    wait for 10 us;

    --------------------------------------------------------------------
    -- Send READ_DEFAULT_REG command:
    -- TX: AA 01 00 AB 55   (CHK = AA xor 01 xor 00 = AB)
    --------------------------------------------------------------------
    report "Sending READ_DEFAULT_REG: AA 01 00 AB 55" severity note;

    uart_send_byte(uart_rx, x"AA");
    uart_send_byte(uart_rx, x"01");
    uart_send_byte(uart_rx, x"00");
    uart_send_byte(uart_rx, x"AB");
    uart_send_byte(uart_rx, x"55");

    --------------------------------------------------------------------
    -- Receive response bytes and print in HEX:
    -- Expected: CC 01 00 02 23 01 ED 33
    --------------------------------------------------------------------
    uart_recv_byte(uart_tx, b0);
    uart_recv_byte(uart_tx, b1);
    uart_recv_byte(uart_tx, b2);
    uart_recv_byte(uart_tx, b3);
    uart_recv_byte(uart_tx, b4);
    uart_recv_byte(uart_tx, b5);
    uart_recv_byte(uart_tx, b6);
    uart_recv_byte(uart_tx, b7);

    report "Response bytes: "
      & byte_to_hex(b0) & " "
      & byte_to_hex(b1) & " "
      & byte_to_hex(b2) & " "
      & byte_to_hex(b3) & " "
      & byte_to_hex(b4) & " "
      & byte_to_hex(b5) & " "
      & byte_to_hex(b6) & " "
      & byte_to_hex(b7) severity note;

    -- Hard checks
    assert b0 = x"CC" report "Bad SOF (expected CC)" severity failure;
    assert b1 = x"01" report "Bad CMD (expected 01)" severity failure;
    assert b2 = x"00" report "Bad STATUS (expected 00)" severity failure;
    assert b3 = x"02" report "Bad LEN (expected 02)" severity failure;
    assert b4 = x"23" report "Bad DATA[0] (expected 23)" severity failure;
    assert b5 = x"01" report "Bad DATA[1] (expected 01)" severity failure;
    assert b6 = x"ED" report "Bad CHK (expected ED)" severity failure;
    assert b7 = x"33" report "Bad EOF (expected 33)" severity failure;

    report "READ_DEFAULT_REG test PASSED" severity note;

    wait for 200 us;
    assert false report "Simulation finished" severity failure;
  end process;

end architecture;