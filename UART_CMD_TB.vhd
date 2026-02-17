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

  -- IMPORTANT: match DUT bit time based on integer BAUD_DIV
  constant BAUD_DIV   : integer := CLK_HZ / BAUD;         -- 434
  constant BIT_TIME   : time := BAUD_DIV * CLK_PERIOD;    -- 434 * 20ns = 8680 ns

  -- DUT signals
  signal clk50    : std_logic := '0';
  signal reset_n  : std_logic := '0';
  signal uart_rx  : std_logic := '1';
  signal uart_tx  : std_logic;
  signal led_dout : std_logic;

  -- VHDL-93 compatible hex helpers (no to_hstring)
  function hex_nibble(n : std_logic_vector(3 downto 0)) return character is
  begin
    case n is
      when "0000" => return '0';
      when "0001" => return '1';
      when "0010" => return '2';
      when "0011" => return '3';
      when "0100" => return '4';
      when "0101" => return '5';
      when "0110" => return '6';
      when "0111" => return '7';
      when "1000" => return '8';
      when "1001" => return '9';
      when "1010" => return 'A';
      when "1011" => return 'B';
      when "1100" => return 'C';
      when "1101" => return 'D';
      when "1110" => return 'E';
      when "1111" => return 'F';
      when others => return 'X';
    end case;
  end function;

  function byte_to_hex(b : std_logic_vector(7 downto 0)) return string is
    variable s : string(1 to 2);
  begin
    s(1) := hex_nibble(b(7 downto 4));
    s(2) := hex_nibble(b(3 downto 0));
    return s;
  end function;

  -- UART transmit byte onto uart_rx line (8N1, LSB first)
  procedure uart_send_byte(signal line : out std_logic; b : std_logic_vector(7 downto 0)) is
  begin
    line <= '0';                       -- start bit
    wait for BIT_TIME;

    for i in 0 to 7 loop               -- data bits LSB first
      line <= b(i);
      wait for BIT_TIME;
    end loop;

    line <= '1';                       -- stop bit
    wait for BIT_TIME;
  end procedure;

  -- Robust UART receive byte from uart_tx line (8N1, LSB first)
  -- Uses falling edge of start bit and samples at bit centers.
  procedure uart_recv_byte(signal line : in std_logic; variable b : out std_logic_vector(7 downto 0)) is
  begin
    -- Wait for a real falling edge (start bit)
    wait until (line = '0' and line'event);

    -- Go to center of bit0: 1.5 bit times from start edge
    wait for BIT_TIME + (BIT_TIME/2);

    -- Sample 8 data bits at center
    for i in 0 to 7 loop
      b(i) := line;
      wait for BIT_TIME;
    end loop;

    -- Now we are at center of stop bit (because we waited BIT_TIME after bit7)
    if line /= '1' then
      report "UART stop bit not high" severity warning;
    end if;

    -- Move to end of stop bit (optional)
    wait for BIT_TIME/2;
  end procedure;

begin
  -- Clock generation
  clk50 <= not clk50 after CLK_PERIOD/2;

  -- DUT instance (must match your entity name and ports)
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

  stim: process
    variable idx  : std_logic_vector(7 downto 0);
    variable g    : std_logic_vector(7 downto 0);
    variable r    : std_logic_vector(7 downto 0);
    variable bb   : std_logic_vector(7 downto 0); -- use bb to avoid name clash with signal b
    variable w    : std_logic_vector(7 downto 0);
    variable chk  : std_logic_vector(7 downto 0);

    variable ack0 : std_logic_vector(7 downto 0);
    variable ack1 : std_logic_vector(7 downto 0);
    variable ack2 : std_logic_vector(7 downto 0);
    variable ack3 : std_logic_vector(7 downto 0);
    variable ack4 : std_logic_vector(7 downto 0);
    variable ack5 : std_logic_vector(7 downto 0);

    variable resp_chk : std_logic_vector(7 downto 0);
  begin
    -- Reset
    reset_n <= '0';
    uart_rx <= '1';
    wait for 2 us;
    reset_n <= '1';
    wait for 2 us;

    -- Build a valid command: set LED0 to BLUE (G=0,R=0,B=255,W=0)
    idx := x"00";
    g   := x"00";
    r   := x"00";
    bb  := x"FF";
    w   := x"00";

    -- chk = XOR(bytes 0..5) where byte0 is 0xAA
    chk := x"AA" xor idx xor g xor r xor bb xor w;

    -- Send frame: AA idx G R B W chk 55
    uart_send_byte(uart_rx, x"AA");
    uart_send_byte(uart_rx, idx);
    uart_send_byte(uart_rx, g);
    uart_send_byte(uart_rx, r);
    uart_send_byte(uart_rx, bb);
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
      & byte_to_hex(ack0) & " "
      & byte_to_hex(ack1) & " "
      & byte_to_hex(ack2) & " "
      & byte_to_hex(ack3) & " "
      & byte_to_hex(ack4) & " "
      & byte_to_hex(ack5);

    -- Checks
    assert ack0 = x"CC" report "ACK[0] not 0xCC" severity failure;
    assert ack5 = x"33" report "ACK[5] not 0x33" severity failure;
    assert ack1 = idx   report "ACK idx mismatch" severity failure;

    assert ack2 = x"00" report "ACK status not OK (0x00)" severity failure;
    assert ack3 = chk   report "ACK echo_chk mismatch" severity failure;

    resp_chk := ack0 xor ack1 xor ack2 xor ack3;
    assert ack4 = resp_chk report "ACK resp_chk mismatch" severity failure;

    report "UART RX + ACK test PASSED" severity note;

    wait for 200 us;
    assert false report "Simulation finished" severity failure;
  end process;

end architecture;