library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;


entity TestEncoderTop is
port(
    led:    out Std_Logic_Vector(0 to 3) := "1111";
    switch: in  Std_Logic_Vector(0 to 1);
	 disp7seg_dig: out Std_Logic_Vector(3 downto 0) := "1111";
	 disp7seg_seg: out Std_Logic_Vector(0 to 7) := "11111111";
	 clk: in Std_Logic;
	 reset: in Std_Logic
	 );
end TestEncoderTop;

architecture Behavioral of TestEncoderTop is

TYPE State_type IS (Idle, Left1, Left2, Left3, Right1, Right2, Right3);  -- Define the states
SIGNAL state : State_Type := Idle;    -- Create a signal that uses the different states

constant CLK_FREQ : integer := 50000000;
constant FREQ_7SEG : integer := 400;
constant FREQ_7SEG_CNT_MAX : integer := CLK_FREQ/FREQ_7SEG/2 - 1;
constant FREQ_ENCODER : integer := 5000;
constant FREQ_ENCODER_CNT_MAX : integer := CLK_FREQ/FREQ_ENCODER/2 - 1;
constant FREQ_COUNTER : integer := 250;
constant FREQ_COUNTER_CNT_MAX : integer := CLK_FREQ/FREQ_COUNTER/2 - 1;

signal FREQ_7SEG_CNT : unsigned(24 downto 0);
signal FREQ_ENCODER_CNT : unsigned(24 downto 0);
signal FREQ_COUNTER_CNT : unsigned(24 downto 0);

signal FREQ_7SEG_SIGNAL : Std_Logic;
signal FREQ_ENCODER_SIGNAL : Std_Logic;
signal FREQ_COUNTER_SIGNAL : Std_Logic;

signal disp_7seg_counter : Std_Logic_Vector(15 downto 0) := x"0000";
signal disp_7seg_digit : Std_Logic_Vector(3 downto 0);

signal digit : unsigned(1 downto 0);

begin

process (clk,reset)
begin
	if (clk='1' and clk'event) then
		if FREQ_7SEG_CNT = FREQ_7SEG_CNT_MAX then
			FREQ_7SEG_CNT <= (others => '0');
			FREQ_7SEG_SIGNAL <= not FREQ_7SEG_SIGNAL;
		else
			FREQ_7SEG_CNT <= FREQ_7SEG_CNT + 1;
		end if;
	end if; 
end process;

process (clk,reset)
begin
	if (reset='0') then
		FREQ_ENCODER_CNT <= (others => '0');
		FREQ_ENCODER_SIGNAL <= '0';
	elsif (clk='1' and clk'event) then
		if FREQ_ENCODER_CNT = FREQ_ENCODER_CNT_MAX then
			FREQ_ENCODER_CNT <= (others => '0');
			FREQ_ENCODER_SIGNAL <= not FREQ_ENCODER_SIGNAL;
      else
			FREQ_ENCODER_CNT <= FREQ_ENCODER_CNT + 1;
      end if;
	end if; 
end process;

process (clk,reset)
begin
	if (reset='0') then
		FREQ_COUNTER_CNT <= (others => '0');
		FREQ_COUNTER_SIGNAL <= '0';
	elsif (clk='1' and clk'event) then
		if FREQ_COUNTER_CNT = FREQ_COUNTER_CNT_MAX then
			FREQ_COUNTER_CNT <= (others => '0');
			FREQ_COUNTER_SIGNAL <= not FREQ_COUNTER_SIGNAL;
		else
			FREQ_COUNTER_CNT <= FREQ_COUNTER_CNT + 1;
		end if;
	end if;
end process;

process(state,switch)
begin

	case state is
		when Idle =>
			case switch is
				when "01" => state <= Left1;
				when "10" => state <= Right1;
				when others => state <= state;
			end case;
      when Left1 =>
			case switch is
				when "00" => state <= Left2;
				when "11" => state <= Idle;
				when others => state <= state;
			end case;
      when Left2 =>
			case switch is
				when "01" => state <= Left1;
				when "10" => state <= Left3;
				when others => state <= state;
			end case;
		when Left3 =>
			case switch is
				when "00" => state <= Left2;
				when "11" => state <= Idle;
				when others => state <= state;
			end case;
      when Right1 =>
			case switch is
				when "00" => state <= Right2;
				when "11" => state <= Idle;
				when others => state <= state;
			end case;
      when Right2 =>
			case switch is
				when "10" => state <= Right1;
				when "01" => state <= Right3;
				when others => state <= state;
			end case;
      when Right3 =>
			case switch is
				when "00" => state <= Right2;
				when "11" => state <= Idle;
				when others => state <= state;
			end case;
      end case;

end process;


led <= "1111" when state = Idle else
       "0111" when state = Left1 else
       "0011" when state = Left2 else
       "0001" when state = Left3 else
       "1110" when state = Right1 else
       "1100" when state = Right2 else
       "1000" when state = Right3 else
       "0110"; -- error

process(FREQ_7SEG_SIGNAL)
begin
	if (FREQ_7SEG_SIGNAL='1' and FREQ_7SEG_SIGNAL'event) then
		digit <= digit + 1;
	end if;
end process;

process(FREQ_COUNTER_SIGNAL,reset)
begin
	if (reset='0') then
		disp_7seg_counter <= (others => '0');
	elsif (FREQ_COUNTER_SIGNAL='0' and FREQ_COUNTER_SIGNAL'event) then
		disp_7seg_counter <= disp_7seg_counter + '1';
	end if;
end process;

disp_7seg_digit <= disp_7seg_counter(15 downto 12) when digit = 0 else
                   disp_7seg_counter(11 downto  8) when digit = 1 else
                   disp_7seg_counter( 7 downto  4) when digit = 2 else
                   disp_7seg_counter( 3 downto  0);

disp7seg_dig <= "0111" when digit = 0 and ((not (disp_7seg_counter(15 downto 12) = "0000")) or (reset = '0')) else
                "1111" when digit = 0 else
                "1011" when digit = 1 and ((not (disp_7seg_counter(15 downto  8) = "00000000")) or (reset = '0')) else
                "1111" when digit = 1 else
                "1101" when digit = 2 and ((not (disp_7seg_counter(15 downto  4) = "000000000000")) or (reset = '0')) else
                "1111" when digit = 2 else
                "1110";

disp7seg_seg(7) <= '1';
disp7seg_seg(0 to 6) <= "1111110" when reset = '0' else
                        "0000001" when disp_7seg_digit = "0000" else -- "0"
                        "1001111" when disp_7seg_digit = "0001" else -- "1"
                        "0010010" when disp_7seg_digit = "0010" else -- "2"
                        "0000110" when disp_7seg_digit = "0011" else -- "3"
                        "1001100" when disp_7seg_digit = "0100" else -- "4"
                        "0100100" when disp_7seg_digit = "0101" else -- "5"
                        "0100000" when disp_7seg_digit = "0110" else -- "6"
                        "0001111" when disp_7seg_digit = "0111" else -- "7"
                        "0000000" when disp_7seg_digit = "1000" else -- "8"
                        "0000100" when disp_7seg_digit = "1001" else -- "9"
                        "0001000" when disp_7seg_digit = "1010" else -- "a"
                        "1100000" when disp_7seg_digit = "1011" else -- "b"
                        "0110001" when disp_7seg_digit = "1100" else -- "C"
                        "1000010" when disp_7seg_digit = "1101" else -- "d"
                        "0110000" when disp_7seg_digit = "1110" else -- "E"
                        "0111000" when disp_7seg_digit = "1111" else -- "F"
                        "0110110";

end Behavioral;
