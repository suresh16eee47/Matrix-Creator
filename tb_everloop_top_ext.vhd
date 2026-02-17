library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- ============================================================
-- Testbench: READ_DEFAULT_REG (0x2301)
-- Sends:    AA 01 00 AB 55
-- Expects:  CC 01 00 02 23 01 ED 33
--
-- Robust UART RX decoder in TB:
--  - waits for idle-high
--  - validates start bit (still low at mid-start)
--  - samples data at centers
--  - samples stop at center (prevents false warnings on back-to-back bytes)
-- ============================================================

entity tb_read_default_reg is
end entity;

architecture sim of tb_read_default_reg is
  constant CLK_HZ : integer := 50_000_000;
  constant BAUD   : integer := 115200;

  constant CLK_PERIOD : time := 20 ns;
  constant BAUD_DIV   : integer := CLK_HZ / BAUD;          -- 434
  constant BIT_TIME   : time := BAUD_DIV * CLK_PERIOD;     -- 8.68 us

  signal clk50    : std_logic := '0';
  signal reset_n  : std_logic := '0';
  signal uart_rx  : std_logic := '1';  -- idle high
  signal uart_tx  : std_logic;
  signal led_dout : std_logic;

  -- ---------- HEX helpers (VHDL-93 compatible) ----------
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

  -- ---------- UART helpers ----------
  procedure uart_send_byte(signal line : out std_logic; b : std_logic_vector(7 downto 0)) is
  begin
    -- start
    line <= '0';
    wait for BIT_TIME;

    -- data LSB first
    for i in 0 to 7 loop
      line <= b(i);
      wait for BIT_TIME;
    end loop;

    -- stop
    line <= '1';
    wait for BIT_TIME;
  end procedure;

  -- Robust receive: avoids false "stop bit low" due to edge mis-lock/drift
  procedure uart_recv_byte(signal line : in std_logic; variable b : out std_logic_vector(7 downto 0)) is
  begin
    -- Ensure idle high before searching start bit
    if line /= '1' then
      wait until line = '1';
    end if;

    -- Find a VALID start bit (falling edge + still low at mid-start)
    loop
      wait until (line'event and line = '0');  -- candidate start edge
      wait for BIT_TIME/2;                     -- middle of start bit
      exit when line = '0';                    -- accept only if still low
      -- otherwise it was a data-edge/glitch, continue searching
    end loop;

    -- Now go to center of bit0 (from mid-start -> mid-bit0 is +1 bit)
    wait for BIT_TIME;

    -- Sample 8 data bits at centers (LSB first)
    for i in 0 to 7 loop
      b(i) := line;
      wait for BIT_TIME;
    end loop;

    -- We are now at START of stop bit; sample at CENTER of stop bit
    wait for BIT_TIME/2;
    if line /= '1' then
      report "Stop bit not high (sampling)" severity warning;
    end if;
    wait for BIT_TIME/2;
  end procedure;

begin
  -- clock generation
  clk50 <= not clk50 after CLK_PERIOD/2;

  -- DUT instantiation
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
    variable r0,r1,r2,r3,r4,r5,r6,r7 : std_logic_vector(7 downto 0);
  begin
    uart_rx <= '1';

    -- reset
    reset_n <= '0';
    wait for 20 us;
    reset_n <= '1';
    wait for 20 us;

    -- Send READ_DEFAULT_REG: AA 01 00 AB 55
    report "TX: AA 01 00 AB 55 (READ_DEFAULT_REG)" severity note;
    uart_send_byte(uart_rx, x"AA");
    uart_send_byte(uart_rx, x"01");
    uart_send_byte(uart_rx, x"00");
    uart_send_byte(uart_rx, x"AB");
    uart_send_byte(uart_rx, x"55");

    -- Receive response: CC 01 00 02 23 01 ED 33
    uart_recv_byte(uart_tx, r0);
    uart_recv_byte(uart_tx, r1);
    uart_recv_byte(uart_tx, r2);
    uart_recv_byte(uart_tx, r3);
    uart_recv_byte(uart_tx, r4);
    uart_recv_byte(uart_tx, r5);
    uart_recv_byte(uart_tx, r6);
    uart_recv_byte(uart_tx, r7);

    report "RX: "
      & byte_to_hex(r0) & " "
      & byte_to_hex(r1) & " "
      & byte_to_hex(r2) & " "
      & byte_to_hex(r3) & " "
      & byte_to_hex(r4) & " "
      & byte_to_hex(r5) & " "
      & byte_to_hex(r6) & " "
      & byte_to_hex(r7) severity note;

    -- Assertions
    assert r0 = x"CC" report "Bad SOF (expected CC)" severity failure;
    assert r1 = x"01" report "Bad CMD (expected 01)" severity failure;
    assert r2 = x"00" report "Bad STATUS (expected 00)" severity failure;
    assert r3 = x"02" report "Bad LEN (expected 02)" severity failure;
    assert r4 = x"23" report "Bad DATA[0] (expected 23)" severity failure;
    assert r5 = x"01" report "Bad DATA[1] (expected 01)" severity failure;
    assert r6 = x"ED" report "Bad CHK (expected ED)" severity failure;
    assert r7 = x"33" report "Bad EOF (expected 33)" severity failure;

    report "READ_DEFAULT_REG test PASSED" severity note;

    wait for 200 us;
    assert false report "Simulation finished" severity failure;
  end process;

end architecture;