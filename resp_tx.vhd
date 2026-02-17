library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity resp_tx is
  generic (
    CLK_HZ    : natural := 50_000_000;
    BAUD      : natural := 115200;
    MAX_RESP  : natural := 32
  );
  port (
    clk        : in  std_logic;
    reset_n    : in  std_logic;

    -- request (1 pulse)
    req        : in  std_logic;
    cmd        : in  std_logic_vector(7 downto 0);
    status     : in  std_logic_vector(7 downto 0);
    len        : in  std_logic_vector(7 downto 0);
    data       : in  std_logic_vector(MAX_RESP*8-1 downto 0);

    -- UART TX link
    tx_busy    : in  std_logic;
    tx_start   : out std_logic;
    tx_byte    : out std_logic_vector(7 downto 0);

    busy       : out std_logic  -- busy sending a response frame
  );
end entity;

architecture rtl of resp_tx is
  type st_t is (IDLE, B0, B1, B2, B3, DATA, CHK, EOF);
  signal st : st_t := IDLE;

  signal r_cmd    : std_logic_vector(7 downto 0) := (others=>'0');
  signal r_status : std_logic_vector(7 downto 0) := (others=>'0');
  signal r_len    : unsigned(7 downto 0) := (others=>'0');
  signal r_data   : std_logic_vector(MAX_RESP*8-1 downto 0) := (others=>'0');

  signal idx      : unsigned(7 downto 0) := (others=>'0');
  signal xoracc   : std_logic_vector(7 downto 0) := (others=>'0');

  function get_byte(v : std_logic_vector; i : integer) return std_logic_vector is
    variable ofs : integer;
  begin
    ofs := i*8;
    return v(ofs+7 downto ofs);
  end function;

begin
  process(clk)
    variable L : integer;
    variable b : std_logic_vector(7 downto 0);
  begin
    if rising_edge(clk) then
      tx_start <= '0';
      busy     <= '0';

      if reset_n = '0' then
        st     <= IDLE;
        r_cmd  <= (others=>'0');
        r_status <= (others=>'0');
        r_len  <= (others=>'0');
        r_data <= (others=>'0');
        idx    <= (others=>'0');
        xoracc <= (others=>'0');
        tx_byte<= (others=>'0');
      else
        case st is
          when IDLE =>
            if req = '1' then
              r_cmd    <= cmd;
              r_status <= status;
              r_len    <= unsigned(len);
              r_data   <= data;
              xoracc   <= x"CC" xor cmd xor status xor len;
              idx      <= (others=>'0');
              st       <= B0;
            end if;

          when B0 =>
            busy <= '1';
            if tx_busy = '0' then tx_byte <= x"CC"; tx_start <= '1'; st <= B1; end if;

          when B1 =>
            busy <= '1';
            if tx_busy = '0' then tx_byte <= r_cmd; tx_start <= '1'; st <= B2; end if;

          when B2 =>
            busy <= '1';
            if tx_busy = '0' then tx_byte <= r_status; tx_start <= '1'; st <= B3; end if;

          when B3 =>
            busy <= '1';
            if tx_busy = '0' then tx_byte <= std_logic_vector(r_len); tx_start <= '1';
              if r_len = 0 then
                st <= CHK;
              else
                st <= DATA;
              end if;
            end if;

          when DATA =>
            busy <= '1';
            L := to_integer(r_len);
            if tx_busy = '0' then
              b := get_byte(r_data, to_integer(idx));
              tx_byte  <= b;
              tx_start <= '1';
              xoracc   <= xoracc xor b;
              if to_integer(idx) = L-1 then
                st <= CHK;
              else
                idx <= idx + 1;
              end if;
            end if;

          when CHK =>
            busy <= '1';
            if tx_busy = '0' then tx_byte <= xoracc; tx_start <= '1'; st <= EOF; end if;

          when EOF =>
            busy <= '1';
            if tx_busy = '0' then tx_byte <= x"33"; tx_start <= '1'; st <= IDLE; end if;
        end case;
      end if;
    end if;
  end process;
end architecture;