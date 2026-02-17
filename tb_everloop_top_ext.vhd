library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_everloop_top_ext is
end entity;

architecture sim of tb_everloop_top_ext is
  constant CLK_HZ : integer := 50_000_000;
  constant BAUD   : integer := 115200;
  constant N_LEDS : integer := 31;
  constant MAX_PAYLOAD : integer := 32;
  constant MAX_RESP    : integer := 32;

  constant CLK_PERIOD : time := 20 ns;
  constant BAUD_DIV   : integer := CLK_HZ / BAUD;         -- 434
  constant BIT_TIME   : time := BAUD_DIV * CLK_PERIOD;    -- 8680 ns

  signal clk50    : std_logic := '0';
  signal reset_n  : std_logic := '0';
  signal uart_rx  : std_logic := '1';
  signal uart_tx  : std_logic;
  signal led_dout : std_logic;

  -- UART helpers
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

  procedure uart_recv_byte(signal line : in std_logic; variable b : out std_logic_vector(7 downto 0)) is
  begin
    -- wait start edge
    wait until (line = '0' and line'event);

    -- to center of bit0 (1.5 bit)
    wait for BIT_TIME + (BIT_TIME/2);

    for i in 0 to 7 loop
      b(i) := line;
      wait for BIT_TIME;
    end loop;

    -- stop bit center
    assert line = '1' report "Stop bit not high (sampling)" severity warning;
    wait for BIT_TIME/2;
  end procedure;

begin
  -- clock
  clk50 <= not clk50 after CLK_PERIOD/2;

  -- DUT
  dut: entity work.everloop_top_ext
    generic map (
      CLK_HZ      => CLK_HZ,
      BAUD        => BAUD,
      N_LEDS      => N_LEDS,
      MAX_PAYLOAD => MAX_PAYLOAD,
      MAX_RESP    => MAX_RESP
    )
    port map (
      clk50    => clk50,
      reset_n  => reset_n,
      uart_rx  => uart_rx,
      uart_tx  => uart_tx,
      led_dout => led_dout
    );

  stim: process
    variable sof, eof  : std_logic_vector(7 downto 0);
    variable cmd, len  : std_logic_vector(7 downto 0);
    variable p0,p1,p2,p3,p4 : std_logic_vector(7 downto 0);
    variable chk : std_logic_vector(7 downto 0);

    variable r0,r1,r2,r3,r4,r5 : std_logic_vector(7 downto 0);
    variable exp_chk : std_logic_vector(7 downto 0);
  begin
    uart_rx <= '1';

    -- reset
    reset_n <= '0';
    wait for 5 us;
    reset_n <= '1';
    wait for 5 us;

    -- Build SET_LED (0x10) LEN=5 payload: idx=0, G=0, R=0, B=FF, W=0
    sof := x"AA";
    eof := x"55";
    cmd := x"10";
    len := x"05";
    p0  := x"00"; -- idx
    p1  := x"00"; -- G
    p2  := x"00"; -- R
    p3  := x"FF"; -- B
    p4  := x"00"; -- W

    chk := sof xor cmd xor len xor p0 xor p1 xor p2 xor p3 xor p4;

    -- Send RX frame: AA CMD LEN payload... CHK 55
    uart_send_byte(uart_rx, sof);
    uart_send_byte(uart_rx, cmd);
    uart_send_byte(uart_rx, len);
    uart_send_byte(uart_rx, p0);
    uart_send_byte(uart_rx, p1);
    uart_send_byte(uart_rx, p2);
    uart_send_byte(uart_rx, p3);
    uart_send_byte(uart_rx, p4);
    uart_send_byte(uart_rx, chk);
    uart_send_byte(uart_rx, eof);

    -- Receive TX response:
    -- CC CMD STATUS LEN DATA[LEN] CHK 33
    uart_recv_byte(uart_tx, r0); -- CC
    uart_recv_byte(uart_tx, r1); -- CMD
    uart_recv_byte(uart_tx, r2); -- STATUS
    uart_recv_byte(uart_tx, r3); -- LEN
    -- LEN=0 expected for SET_LED OK, so next is CHK then 33
    uart_recv_byte(uart_tx, r4); -- CHK
    uart_recv_byte(uart_tx, r5); -- 33

    -- Check fixed fields
    assert r0 = x"CC" report "Bad response SOF (expected CC)" severity failure;
    assert r1 = cmd   report "Bad response CMD" severity failure;
    assert r2 = x"00" report "Bad response STATUS (expected 00)" severity failure;
    assert r3 = x"00" report "Bad response LEN (expected 00)" severity failure;
    assert r5 = x"33" report "Bad response EOF (expected 33)" severity failure;

    -- Check XOR checksum: XOR(CC, CMD, STATUS, LEN)
    exp_chk := x"CC" xor r1 xor r2 xor r3;
    assert r4 = exp_chk report "Bad response CHK" severity failure;

    report "UART protocol test PASSED" severity note;

    -- Let LED frame run (optional)
    wait for 2 ms;

    assert false report "Simulation finished" severity failure;
  end process;

end architecture;