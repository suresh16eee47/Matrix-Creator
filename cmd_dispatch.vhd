library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cmd_dispatch is
  generic (
    N_LEDS      : natural := 31;
    MAX_PAYLOAD : natural := 32;
    MAX_RESP    : natural := 32
  );
  port (
    clk         : in  std_logic;
    reset_n     : in  std_logic;

    frame_valid : in  std_logic;
    cmd         : in  std_logic_vector(7 downto 0);
    len         : in  std_logic_vector(7 downto 0);
    payload     : in  std_logic_vector(MAX_PAYLOAD*8-1 downto 0);

    err_valid   : in  std_logic;
    err_cmd     : in  std_logic_vector(7 downto 0);
    err_code    : in  std_logic_vector(7 downto 0);

    -- LED write outputs (GRBW packed)
    led_wr_pulse: out std_logic;
    led_all_pulse:out std_logic;
    led_idx_nat : out natural range 0 to N_LEDS-1;
    led_pix_grbw: out std_logic_vector(31 downto 0);

    -- Response request (to resp_tx)
    resp_req    : out std_logic; -- 1 clk pulse
    resp_cmd    : out std_logic_vector(7 downto 0);
    resp_status : out std_logic_vector(7 downto 0);
    resp_len    : out std_logic_vector(7 downto 0);
    resp_data   : out std_logic_vector(MAX_RESP*8-1 downto 0)
  );
end entity;

architecture rtl of cmd_dispatch is
  function get_byte(v : std_logic_vector; i : integer) return std_logic_vector is
    variable b : std_logic_vector(7 downto 0);
    variable ofs : integer;
  begin
    ofs := i*8;
    b := v(ofs+7 downto ofs);
    return b;
  end function;

  signal r_data : std_logic_vector(MAX_RESP*8-1 downto 0) := (others=>'0');
begin
  resp_data <= r_data;

  process(clk)
    variable L : integer;
    variable idx_u : integer;
    variable g,r,b,w : std_logic_vector(7 downto 0);
  begin
    if rising_edge(clk) then
      if reset_n = '0' then
        led_wr_pulse  <= '0';
        led_all_pulse <= '0';
        led_idx_nat   <= 0;
        led_pix_grbw  <= (others=>'0');

        resp_req      <= '0';
        resp_cmd      <= (others=>'0');
        resp_status   <= (others=>'0');
        resp_len      <= (others=>'0');
        r_data        <= (others=>'0');
      else
        led_wr_pulse  <= '0';
        led_all_pulse <= '0';
        resp_req      <= '0';

        -- Handle framing errors (checksum/end/len)
        if err_valid = '1' then
          resp_cmd    <= err_cmd;
          resp_status <= err_code;        -- reuse codes: 01/02/03
          resp_len    <= x"00";
          r_data      <= (others=>'0');
          resp_req    <= '1';
        end if;

        -- Handle valid frames
        if frame_valid = '1' then
          L := to_integer(unsigned(len));
          r_data <= (others=>'0');
          resp_cmd <= cmd;

          case cmd is
            when x"10" =>  -- SET_LED: idx,G,R,B,W (LEN=5)
              if L /= 5 then
                resp_status <= x"06"; -- BAD_PARAM
                resp_len    <= x"00";
              else
                idx_u := to_integer(unsigned(get_byte(payload,0)));
                if idx_u < integer(N_LEDS) then
                  g := get_byte(payload,1);
                  r := get_byte(payload,2);
                  b := get_byte(payload,3);
                  w := get_byte(payload,4);

                  led_idx_nat  <= idx_u;
                  led_pix_grbw <= g & r & b & w;
                  led_wr_pulse <= '1';

                  resp_status <= x"00";
                  resp_len    <= x"00";
                else
                  resp_status <= x"03"; -- BAD_INDEX
                  resp_len    <= x"00";
                end if;
              end if;
              resp_req <= '1';

            when x"11" =>  -- SET_ALL: G,R,B,W (LEN=4)
              if L /= 4 then
                resp_status <= x"06";
                resp_len    <= x"00";
              else
                g := get_byte(payload,0);
                r := get_byte(payload,1);
                b := get_byte(payload,2);
                w := get_byte(payload,3);
                led_pix_grbw  <= g & r & b & w;
                led_all_pulse <= '1';
                resp_status   <= x"00";
                resp_len      <= x"00";
              end if;
              resp_req <= '1';

            when x"20" =>  -- MAG_READ (stub): return 6 bytes XYZ (all 0)
              resp_status <= x"00";
              resp_len    <= x"06";
              -- data[0..5] = 0
              r_data(7 downto 0)    <= x"00";
              r_data(15 downto 8)   <= x"00";
              r_data(23 downto 16)  <= x"00";
              r_data(31 downto 24)  <= x"00";
              r_data(39 downto 32)  <= x"00";
              r_data(47 downto 40)  <= x"00";
              resp_req <= '1';

            when others =>
              resp_status <= x"04"; -- UNKNOWN_CMD
              resp_len    <= x"00";
              resp_req    <= '1';
          end case;
        end if;
      end if;
    end if;
  end process;
end architecture;