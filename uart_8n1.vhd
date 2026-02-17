library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- ============================================================
-- UART RX 8N1
-- ============================================================
entity uart_rx_8n1 is
  generic (
    CLK_HZ : natural := 50_000_000;
    BAUD   : natural := 115200
  );
  port (
    clk        : in  std_logic;
    reset_n    : in  std_logic;  -- active low
    rx         : in  std_logic;  -- idle high
    data_out   : out std_logic_vector(7 downto 0);
    data_valid : out std_logic   -- 1 clk pulse
  );
end entity;

architecture rtl of uart_rx_8n1 is
  constant BAUD_DIV : natural := CLK_HZ / BAUD;

  type rx_state_t is (RX_IDLE, RX_SBIT, RX_DATA, RX_PBIT);
  signal state  : rx_state_t := RX_IDLE;

  signal divcnt : natural range 0 to BAUD_DIV := 0;
  signal bitcnt : integer range 0 to 7 := 0;
  signal shift  : std_logic_vector(7 downto 0) := (others => '0');

  -- 2FF sync
  signal rx_ff1, rx_ff2 : std_logic := '1';
  signal rx_sync        : std_logic := '1';
begin
  -- Synchronize RX
  process(clk)
  begin
    if rising_edge(clk) then
      rx_ff1  <= rx;
      rx_ff2  <= rx_ff1;
      rx_sync <= rx_ff2;
    end if;
  end process;

  -- RX FSM (mid-bit sampling)
  process(clk)
  begin
    if rising_edge(clk) then
      data_valid <= '0';

      if reset_n = '0' then
        state    <= RX_IDLE;
        divcnt   <= 0;
        bitcnt   <= 0;
        shift    <= (others => '0');
        data_out <= (others => '0');
      else
        case state is
          when RX_IDLE =>
            divcnt <= 0;
            if rx_sync = '0' then
              state <= RX_SBIT;
            end if;

          when RX_SBIT =>
            -- sample center of start bit
            if divcnt = BAUD_DIV/2 then
              divcnt <= 0;
              if rx_sync = '0' then
                bitcnt <= 0;
                state  <= RX_DATA;
              else
                state <= RX_IDLE; -- false start
              end if;
            else
              divcnt <= divcnt + 1;
            end if;

          when RX_DATA =>
            if divcnt = BAUD_DIV-1 then
              divcnt <= 0;
              shift(bitcnt) <= rx_sync;  -- LSB first
              if bitcnt = 7 then
                state <= RX_PBIT;        -- stop bit
              else
                bitcnt <= bitcnt + 1;
              end if;
            else
              divcnt <= divcnt + 1;
            end if;

          when RX_PBIT =>
            if divcnt = BAUD_DIV-1 then
              divcnt <= 0;
              if rx_sync = '1' then
                data_out   <= shift;
                data_valid <= '1';
              end if;
              state <= RX_IDLE;
            else
              divcnt <= divcnt + 1;
            end if;
        end case;
      end if;
    end if;
  end process;
end architecture;


-- ============================================================
-- UART TX 8N1
-- ============================================================
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity uart_tx_8n1 is
  generic (
    CLK_HZ : natural := 50_000_000;
    BAUD   : natural := 115200
  );
  port (
    clk     : in  std_logic;
    reset_n : in  std_logic;  -- active low
    start   : in  std_logic;  -- 1 clk pulse when busy=0
    data_in : in  std_logic_vector(7 downto 0);
    busy    : out std_logic;
    tx      : out std_logic   -- idle high
  );
end entity;

architecture rtl of uart_tx_8n1 is
  constant BAUD_DIV : natural := CLK_HZ / BAUD;

  type tx_state_t is (TX_IDLE, TX_SBIT, TX_DATA, TX_PBIT);
  signal state : tx_state_t := TX_IDLE;

  signal divcnt : natural range 0 to BAUD_DIV := 0;
  signal bitcnt : integer range 0 to 7 := 0;
  signal shift  : std_logic_vector(7 downto 0) := (others => '0');
  signal tx_reg : std_logic := '1';
begin
  tx <= tx_reg;

  process(clk)
  begin
    if rising_edge(clk) then
      if reset_n = '0' then
        state  <= TX_IDLE;
        divcnt <= 0;
        bitcnt <= 0;
        shift  <= (others => '0');
        tx_reg <= '1';
        busy   <= '0';
      else
        case state is
          when TX_IDLE =>
            tx_reg <= '1';
            busy   <= '0';
            divcnt <= 0;
            if start = '1' then
              shift <= data_in;
              busy  <= '1';
              state <= TX_SBIT;
            end if;

          when TX_SBIT =>
            tx_reg <= '0';
            busy   <= '1';
            if divcnt = BAUD_DIV-1 then
              divcnt <= 0;
              bitcnt <= 0;
              state  <= TX_DATA;
            else
              divcnt <= divcnt + 1;
            end if;

          when TX_DATA =>
            tx_reg <= shift(bitcnt);
            busy   <= '1';
            if divcnt = BAUD_DIV-1 then
              divcnt <= 0;
              if bitcnt = 7 then
                state <= TX_PBIT;
              else
                bitcnt <= bitcnt + 1;
              end if;
            else
              divcnt <= divcnt + 1;
            end if;

          when TX_PBIT =>
            tx_reg <= '1';  -- stop bit
            busy   <= '1';
            if divcnt = BAUD_DIV-1 then
              divcnt <= 0;
              state  <= TX_IDLE;
            else
              divcnt <= divcnt + 1;
            end if;
        end case;
      end if;
    end if;
  end process;
end architecture;