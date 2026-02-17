library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity uart_frame_rx is
  generic (
    MAX_PAYLOAD : natural := 32
  );
  port (
    clk        : in  std_logic;
    reset_n    : in  std_logic;

    rx_byte    : in  std_logic_vector(7 downto 0);
    rx_strobe  : in  std_logic;

    frame_valid: out std_logic;  -- 1-clk pulse when a full valid frame received
    cmd        : out std_logic_vector(7 downto 0);
    len        : out std_logic_vector(7 downto 0);
    payload    : out std_logic_vector(MAX_PAYLOAD*8-1 downto 0);

    err_valid  : out std_logic;  -- 1-clk pulse on error
    err_cmd    : out std_logic_vector(7 downto 0); -- best effort (last cmd seen)
    err_code   : out std_logic_vector(7 downto 0)  -- 01 bad_chk,02 bad_eof,03 bad_len,04 timeout(not used)
  );
end entity;

architecture rtl of uart_frame_rx is
  type st_t is (S_WAIT_AA, S_CMD, S_LEN, S_PAYLOAD, S_CHK, S_EOF);
  signal st : st_t := S_WAIT_AA;

  signal r_cmd  : std_logic_vector(7 downto 0) := (others=>'0');
  signal r_len  : unsigned(7 downto 0) := (others=>'0');
  signal idx    : unsigned(7 downto 0) := (others=>'0');
  signal xoracc : std_logic_vector(7 downto 0) := (others=>'0');
  signal r_payload : std_logic_vector(MAX_PAYLOAD*8-1 downto 0) := (others=>'0');
begin
  cmd     <= r_cmd;
  len     <= std_logic_vector(r_len);
  payload <= r_payload;

  process(clk)
    variable b : std_logic_vector(7 downto 0);
    variable ofs : integer;
  begin
    if rising_edge(clk) then
      frame_valid <= '0';
      err_valid   <= '0';

      if reset_n = '0' then
        st      <= S_WAIT_AA;
        r_cmd   <= (others=>'0');
        r_len   <= (others=>'0');
        idx     <= (others=>'0');
        xoracc  <= (others=>'0');
        r_payload <= (others=>'0');
        err_cmd <= (others=>'0');
        err_code<= (others=>'0');
      else
        if rx_strobe = '1' then
          b := rx_byte;

          case st is
            when S_WAIT_AA =>
              if b = x"AA" then
                xoracc <= x"AA";
                st     <= S_CMD;
              end if;

            when S_CMD =>
              r_cmd  <= b;
              err_cmd<= b;
              xoracc <= xoracc xor b;
              st     <= S_LEN;

            when S_LEN =>
              r_len  <= unsigned(b);
              xoracc <= xoracc xor b;

              if to_integer(unsigned(b)) > integer(MAX_PAYLOAD) then
                -- bad length (too big)
                err_code  <= x"03";
                err_valid <= '1';
                st        <= S_WAIT_AA;
              else
                idx <= (others=>'0');
                -- clear payload area used
                r_payload <= (others=>'0');
                if b = x"00" then
                  st <= S_CHK;
                else
                  st <= S_PAYLOAD;
                end if;
              end if;

            when S_PAYLOAD =>
              -- store byte into payload(idx)
              ofs := to_integer(idx) * 8;
              r_payload(ofs+7 downto ofs) <= b;
              xoracc <= xoracc xor b;

              if idx = r_len - 1 then
                st <= S_CHK;
              else
                idx <= idx + 1;
              end if;

            when S_CHK =>
              -- compare checksum
              if b /= xoracc then
                err_code  <= x"01";
                err_valid <= '1';
                st        <= S_WAIT_AA;
              else
                st <= S_EOF;
              end if;

            when S_EOF =>
              if b /= x"55" then
                err_code  <= x"02";
                err_valid <= '1';
              else
                frame_valid <= '1';
              end if;
              st <= S_WAIT_AA;

          end case;
        end if;
      end if;
    end if;
  end process;
end architecture;