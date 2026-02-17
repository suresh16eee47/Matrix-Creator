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
    led_wr_pulse : out std_logic;
    led_all_pulse: out std_logic;
    led_idx_nat  : out natural range 0 to N_LEDS-1;
    led_pix_grbw : out std_logic_vector(31 downto 0);

    -- LED read interface (for CMD 0x12)
    led_rd_req     : out std_logic; -- 1 clk pulse
    led_rd_idx_nat : out natural range 0 to N_LEDS-1;
    led_rd_grbw_in : in  std_logic_vector(31 downto 0);

    -- Default register (0x2301) provided by top
    default_reg_in : in  std_logic_vector(15 downto 0);

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
    variable ofs : integer;
  begin
    ofs := i*8;
    return v(ofs+7 downto ofs);
  end function;

  signal r_data : std_logic_vector(MAX_RESP*8-1 downto 0) := (others=>'0');
begin
  resp_data <= r_data;

  process(clk)
    variable L : integer;
    variable idx_u : integer;
    variable g,r,b,w : std_logic_vector(7 downto 0);
    variable grbw : std_logic_vector(31 downto 0);
  begin
    if rising_edge(clk) then
      if reset_n = '0' then
        led_wr_pulse   <= '0';
        led_all_pulse  <= '0';
        led_idx_nat    <= 0;
        led_pix_grbw   <= (others=>'0');

        led_rd_req     <= '0';
        led_rd_idx_nat <= 0;

        resp_req       <= '0';
        resp_cmd       <= (others=>'0');
        resp_status    <= (others=>'0');
        resp_len       <= (others=>'0');
        r_data         <= (others=>'0');
      else
        led_wr_pulse   <= '0';
        led_all_pulse  <= '0';
        led_rd_req     <= '0';
        resp_req       <= '0';

        -- Framing errors -> immediate response
        if err_valid = '1' then
          resp_cmd    <= err_cmd;
          resp_status <= err_code; -- 01 bad_chk, 02 bad_eof, 03 bad_len
          resp_len    <= x"00";
          r_data      <= (others=>'0');
          resp_req    <= '1';
        end if;

        if frame_valid = '1' then
          L := to_integer(unsigned(len));
          r_data   <= (others=>'0');
          resp_cmd <= cmd;

          case cmd is
            ------------------------------------------------------------
            -- 0x10 SET_LED: idx,G,R,B,W  (LEN=5)
            ------------------------------------------------------------
            when x"10" =>
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

            ------------------------------------------------------------
            -- 0x11 SET_ALL: G,R,B,W (LEN=4)
            ------------------------------------------------------------
            when x"11" =>
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

            ------------------------------------------------------------
            -- 0x12 READ_LED: idx (LEN=1) -> DATA LEN=4: G R B W
            ------------------------------------------------------------
            when x"12" =>
              if L /= 1 then
                resp_status <= x"06"; -- BAD_PARAM
                resp_len    <= x"00";
                resp_req    <= '1';
              else
                idx_u := to_integer(unsigned(get_byte(payload,0)));
                if idx_u < integer(N_LEDS) then
                  led_rd_idx_nat <= idx_u;
                  led_rd_req     <= '1';  -- pulse (optional, for future pipelining)

                  -- Read provided by top (combinational or registered)
                  grbw := led_rd_grbw_in;

                  resp_status <= x"00";
                  resp_len    <= x"04";
                  r_data(7 downto 0)    <= grbw(31 downto 24); -- G
                  r_data(15 downto 8)   <= grbw(23 downto 16); -- R
                  r_data(23 downto 16)  <= grbw(15 downto 8);  -- B
                  r_data(31 downto 24)  <= grbw(7 downto 0);   -- W
                  resp_req <= '1';
                else
                  resp_status <= x"03"; -- BAD_INDEX
                  resp_len    <= x"00";
                  resp_req    <= '1';
                end if;
              end if;

            ------------------------------------------------------------
            -- 0x01 READ_DEFAULT_REG: LEN=0 -> DATA LEN=2: 0x23 0x01
            ------------------------------------------------------------
            when x"01" =>
              if L /= 0 then
                resp_status <= x"06"; -- BAD_PARAM
                resp_len    <= x"00";
              else
                resp_status <= x"00";
                resp_len    <= x"02";
                r_data(7 downto 0)   <= default_reg_in(15 downto 8); -- 0x23
                r_data(15 downto 8)  <= default_reg_in(7 downto 0);  -- 0x01
              end if;
              resp_req <= '1';

            ------------------------------------------------------------
            -- 0x20 MAG_READ (stub): returns 6 bytes zeros
            ------------------------------------------------------------
            when x"20" =>
              resp_status <= x"00";
              resp_len    <= x"06";
              r_data(47 downto 0) <= (others => '0');
              resp_req <= '1';

            when others =>
              resp_status <= x"04"; -- UNKNOWN_CMD
              resp_len    <= x"00";
              resp_req <= '1';
          end case;
        end if;
      end if;
    end if;
  end process;

end architecture;