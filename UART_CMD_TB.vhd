library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_everloop_uart is
end entity;

architecture sim of tb_everloop_uart is
  -- Match DUT generics
  constant CLK_HZ : integer := 50_000_000;
  constant BAUD   : integer := 115200;
  constant N_LEDS : integer := 31;

  constant CLK_PERIOD : time := 20 ns;
  constant BIT_TIME   : time := 1 sec / BAUD;  -- ~8.6805 us at 115200

  -- DUT signals
  signal clk50    : std_logic := '0';
  signal reset_n  : std_logic := '0';
  signal uart_rx  : std_logic := '1';
  signal uart_tx  : std_logic;
  signal led_dout : std_logic;

  -- Helpers
  procedure wait_bits(n : natural) is
  begin
    for i in 1 to n loop
      wait for BIT_TIME;
    end loop;
  end procedure;

  -- UART transmit byte onto uart_rx line (8N1, LSB first)
  procedure uart_send_byte(signal line : out std_logic; b : std_logic_vector(7 downto 0)) is
  begin
    -- start bit
    line <= '0';
    wait for BIT_TIME;

    -- 8 data bits, LSB first
    for i in 0 to 7 loop
      line <= b(i);
      wait for BIT_TIME;
    end loop;

    -- stop bit
    line <= '1';
    wait for BIT_TIME;
  end procedure;

  -- UART receive byte from uart_tx line (8N1, LSB first)
  procedure uart_recv_byte(signal line : in std_logic; variable b : out std_logic_vector(7 downto 0)) is
  begin
    -- wait for start bit
    wait until line = '0';
    -- sample in the middle of the start bit, then middle of each data bit
    wait for (BIT_TIME/2);

    -- move to middle of bit0
    wait for BIT_TIME;

    for i in 0 to 7 loop
      b(i) := line;
      wait for BIT_TIME;
    end loop;

    -- stop bit (optional check)
    assert line = '1'
      report "UART stop bit not high!"
      severity warning;

    -- wait to end of stop bit
    wait for BIT_TIME;
  end procedure;

  function xor_bytes(a : std_logic_vector(7 downto 0);
                     b : std_logic_vector(7 downto 0)) return std_logic_vector is
  begin
    return a xor b;
  end function;

begin
  -- Clock generation
  clk50 <= not clk50 after CLK_PERIOD/2;

  -- DUT instance (must match your entity name/ports)
  dut: entity work.everloop_led
    generic map (
      CLK_HZ => CLK_HZ,
      BAUD   => BAUD,
      N_LEDS => N_LEDS
    )
    port map (
      clk50    => clk50,
      reset_n  => reset_n,
      uart_rx  => uart_rx,
      uart_tx  => uart_tx,
      led_dout => led_dout
    );

  -- Test process
  stim: process
    variable idx      : std_logic_vector(7 downto 0);
    variable g, r, b, w : std_logic_vector(7 downto 0);
    variable chk      : std_logic_vector(7 downto 0);

    variable ack0, ack1, ack2, ack3, ack4, ack5 : std_logic_vector(7 downto 0);
    variable resp_chk : std_logic_vector(7 downto 0);
  begin
    -- hold reset low briefly
    reset_n <= '0';
    uart_rx <= '1';
    wait for 200 ns;
    reset_n <= '1';
    wait for 200 ns;

    -- Build a valid command: set LED0 to BLUE (G=0,R=0,B=255,W=0)
    idx := x"00";
    g   := x"00";
    r   := x"00";
    b   := x"FF";
    w   := x"00";

    -- chk = XOR(bytes 0..5) where byte0 is 0xAA
    chk := x"AA" xor idx xor g xor r xor b xor w;

    -- Send frame: AA idx G R B W chk 55
    uart_send_byte(uart_rx, x"AA");
    uart_send_byte(uart_rx, idx);
    uart_send_byte(uart_rx, g);
    uart_send_byte(uart_rx, r);
    uart_send_byte(uart_rx, b);
    uart_send_byte(uart_rx, w);
    uart_send_byte(uart_rx, chk);
    uart_send_byte(uart_rx, x"55");

    -- Receive ACK: CC idx status echo_chk resp_chk 33
    uart_recv_byte(uart_tx, ack0);
    uart_recv_byte(uart_tx, ack1);
    uart_recv_byte(uart_tx, ack2);
    uart_recv_byte(uart_tx, ack3);
    uart_recv_byte(uart_tx, ack4);
    uart_recv_byte(uart_tx, ack5);

    report "ACK bytes: "
      & to_hstring(ack0) & " "
      & to_hstring(ack1) & " "
      & to_hstring(ack2) & " "
      & to_hstring(ack3) & " "
      & to_hstring(ack4) & " "
      & to_hstring(ack5);

    -- Checks
    assert ack0 = x"CC" report "ACK[0] not 0xCC" severity failure;
    assert ack5 = x"33" report "ACK[5] not 0x33" severity failure;
    assert ack1 = idx   report "ACK idx mismatch" severity failure;

    -- status should be 0x00 for OK
    assert ack2 = x"00" report "ACK status not OK (0x00)" severity failure;

    -- echo checksum should match sent chk
    assert ack3 = chk   report "ACK echo_chk mismatch" severity failure;

    -- resp_chk = XOR(CC, idx, status, echo_chk)
    resp_chk := ack0 xor ack1 xor ack2 xor ack3;
    assert ack4 = resp_chk report "ACK resp_chk mismatch" severity failure;

    report "UART RX/ACK test PASSED" severity note;

    -- End simulation
    wait for 1 ms;
    assert false report "Simulation finished" severity failure;
  end process;

end architecture;