library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity vscpu is 
generic( VSCPU_A_WIDTH : integer := 8 ;
			VSCPU_D_WIDTH : integer := 16);
port( clock, reset, start, write_vscpu:in std_logic;
		addr : in std_logic_vector(VSCPU_A_WIDTH-1 downto 0);
		data : in std_logic_vector(VSCPU_D_WIDTH-1 downto 0);
		status : inout std_logic);
end vscpu;

architecture vscpu_arch of vscpu is
type RAM is array(0 to 63) of std_logic_vector(15 downto 0);

signal mem : RAM;
signal data_rd, DR_ff,DR_ns,AC_ff,AC_ns : std_logic_vector(15 downto 0);
signal address,AR_ff,AR_ns,PC_ff,PC_ns : std_logic_vector( 7 downto 0);
signal IR_ff,IR_ns : std_logic_vector(3 downto 0);
signal stvar_ff,stvar_ns : std_logic_vector(7 downto 0);
signal read_vscpu : std_logic;
signal A : std_logic_vector(15 downto 0);

constant INSTR_add : std_logic_vector(3 downto 0) := "0000";
constant INSTR_and : std_logic_vector(3 downto 0) := "0001";
constant INSTR_jmp : std_logic_vector(3 downto 0) := "0010";
constant INSTR_inc : std_logic_vector(3 downto 0) := "0011";
constant INSTR_dcr : std_logic_vector(3 downto 0) := "0100";
constant INSTR_lda : std_logic_vector(3 downto 0) := "0101";
constant INSTR_sub : std_logic_vector(3 downto 0) := "0110";
constant INSTR_mul : std_logic_vector(3 downto 0) := "0111";

constant ST_FETCH1 : std_logic_vector(7 downto 0) := "00000001";
constant ST_FETCH2 : std_logic_vector(7 downto 0) := "00000010";
constant ST_FETCH3 : std_logic_vector(7 downto 0) := "00000011";
constant ST_ADD1 : std_logic_vector(7 downto 0) := "00000100";
constant ST_ADD2 : std_logic_vector(7 downto 0) := "00000101";
constant ST_AND1 : std_logic_vector(7 downto 0) := "00000110";
constant ST_AND2 : std_logic_vector(7 downto 0) := "00000111";
constant ST_JMP1 : std_logic_vector(7 downto 0) := "00001000";
constant ST_INC1 : std_logic_vector(7 downto 0) := "00001001";
constant ST_LDA : std_logic_vector(7 downto 0) := "00001010";
constant ST_DCR1 : std_logic_vector(7 downto 0) := "00001011";
constant ST_SUB1 : std_logic_vector(7 downto 0) := "00001100";
constant ST_SUB2 : std_logic_vector(7 downto 0) := "00001101";

constant  ST_MULT1 : std_logic_vector(7 downto 0) := "00001110";
constant  ST_MULT2 : std_logic_vector(7 downto 0) := "00001111";

constant ST_HALT : std_logic_vector(7 downto 0) := "11111111";
constant PC_STARTS_AT : std_logic_vector(7 downto 0) :=(0=>'1',others => '0');

begin

address <= addr when (ST_HALT = stvar_ff) else AR_ff;

process(clock)
variable ad : integer;
begin
	if(clock'event and clock = '1') then
		if( reset = '1') then
			mem <= (others => (others => '0'));
		elsif( write_vscpu = '1' and read_vscpu = '0') then
			ad := to_integer(unsigned(address));
			mem(ad) <= data;
		elsif( write_vscpu = '0' and read_vscpu = '0' ) then
			ad := to_integer(unsigned(DR_ff(7 downto 0)));
			mem(ad) <= AC_ff;
		end if;
	end if;
end process;

data_rd <= mem(to_integer(unsigned(address))) when (write_vscpu = '0' and read_vscpu = '1') 
			  else (others => 'Z');

status <= '0' when ( write_vscpu = '1' and read_vscpu = '0') else
	  '1' when ( write_vscpu = '0' and read_vscpu = '1') else
	  '0' when ( write_vscpu = '0' and read_vscpu = '0' ) else
	  'U';

process(clock)
begin
	if(clock'event and clock = '1') then
		if( reset = '1') then
			stvar_ff <= ST_HALT;
		elsif( start = '1') then
			stvar_ff <= ST_FETCH1;
		else
			stvar_ff <= stvar_ns;
		end if;
	end if;
end process;
	
process(clock)
begin
	if(clock'event and clock = '1') then
		if( reset = '1') then
			AC_ff <= (others =>'0');
			PC_ff <= PC_STARTS_AT;
			AR_ff <= (others =>'0');
			IR_ff <= (others =>'0');
			DR_ff <= (others =>'0');
		else
			AC_ff <= AC_ns;
			PC_ff <= PC_ns;
			AR_ff <= AR_ns;
			IR_ff <= IR_ns;
			DR_ff <= DR_ns;
		end if;
	end if;
end process;
------------------------------------------------------------------------
process(clock,reset,start,write_vscpu,addr,data)                
begin
	case stvar_ff is
		when ST_HALT => stvar_ns <= ST_HALT;
		when ST_FETCH1 => stvar_ns <= ST_FETCH2;
		when ST_FETCH2 => stvar_ns <= ST_FETCH3;
		when ST_FETCH3=>
			case IR_ff is
				when INSTR_add => stvar_ns <= ST_ADD1;
				when INSTR_and => stvar_ns <= ST_AND1;
				when INSTR_jmp => stvar_ns <= ST_JMP1;
				when INSTR_inc => stvar_ns <= ST_INC1;
				when INSTR_dcr => stvar_ns <= ST_DCR1;
				when INSTR_lda => stvar_ns <= ST_LDA;
				when INSTR_mul => stvar_ns <= ST_MULT1;
				when INSTR_sub => stvar_ns <= ST_SUB1;
				
				when others => null ;
			end case;
		when ST_ADD1 => stvar_ns <= ST_ADD2;
		when ST_AND1 => stvar_ns <= ST_AND2;
		when ST_JMP1 => stvar_ns <= ST_FETCH1;
		when ST_INC1 => stvar_ns <= ST_FETCH1;
		when ST_ADD2 => stvar_ns <= ST_FETCH1;
		when ST_AND2 => stvar_ns <= ST_FETCH1;
		when ST_DCR1 => stvar_ns <= ST_FETCH1;
		when ST_LDA => stvar_ns <= ST_FETCH1;
		when ST_MULT1 => stvar_ns <= ST_MULT2;
		when ST_MULT2 => stvar_ns <= ST_FETCH1;
	   when ST_SUB1 => stvar_ns <= ST_SUB2;
		when ST_SUB2 => stvar_ns <= ST_FETCH1;
		
		when others => stvar_ns <= stvar_ff;
	end case;
end process;

read_vscpu <= '1' when ( ( stvar_ff = ST_FETCH2) or ( stvar_ff = ST_ADD1) or ( stvar_ff = ST_AND1) or ( stvar_ff = ST_LDA) or ( stvar_ff = ST_SUB1) or (stvar_ff=ST_MULT1)) else '0' ;

process(clock,reset,start,write_vscpu,addr,data)
variable ad,X,Y : integer;
variable B : std_logic_vector(32 downto 0);
begin 
	case stvar_ff is
		when ST_FETCH1 => 
								AR_ns <= PC_ff ;
		when ST_FETCH2 => 
								PC_ns <= std_logic_vector(to_unsigned((to_integer(unsigned(PC_ff)) + 1),8)) ; 
								DR_ns <= data_rd ;
								IR_ns <= data_rd( 15 downto 12);
								AR_ns <= data_rd(7 downto 0) ;
		when ST_FETCH3 => null ;
		when ST_ADD1 => 
								DR_ns <= data_rd ;
		when ST_ADD2 => 
								if(clock ='0') then
									AC_ns <= std_logic_vector(to_unsigned(to_integer(unsigned(DR_ns)) + 
										to_integer(unsigned(AC_ff)),16)) ;
								end if;
		when ST_AND1 => 
		
								DR_ns <= data_rd ;
		when ST_AND2 => 
								for i in 0 to 15 loop
									AC_ns(i) <= AC_ns(i) and DR_ns(i);
								end loop;
		when ST_JMP1 => 
								PC_ns <= DR_ff( 7 downto 0);
		when ST_INC1 => 
								AC_ns <= std_logic_vector(to_unsigned(((to_integer(unsigned(AC_ff))) + 1),16));
		when ST_LDA => 
								AC_ns <= data_rd;
		when ST_DCR1 => 
								if (AC_ff = "0000000000000000") then
									AC_ns <= "1111111111111111";
								else
									AC_ns <= std_logic_vector(to_unsigned(((to_integer(unsigned(AC_ff))) - 1),16));
								end if;
		
		
		
		when ST_SUB1 =>
								DR_ns <= data_rd ;
		when ST_SUB2 =>
								X := to_integer(unsigned(AC_ff));
								Y := to_integer(unsigned(DR_ns));
								if(clock ='0') then
									if( X >= Y) then
										X := X - Y;
										AC_ns <= std_logic_vector(to_unsigned(X,16));
									else
										X := 65536 + (X - Y);
										AC_ns <= std_logic_vector(to_unsigned(X,16));
									end if;
								end if;
		
		when ST_MULT1 => DR_ns <= data_rd ;
      when ST_MULT2 => AC_ns <= std_logic_vector((unsigned(AC_ff(7 downto 0))) * (unsigned(DR_ff(7 downto 0))));
					
		
		when others =>
								AR_ns <= AR_ff ; 
								PC_ns <= PC_ff ;
								DR_ns <= DR_ff ; 
								IR_ns <= IR_ff ; 
								AC_ns <= AC_ff ;
		end case;
end process;


 process( clock) begin
 	
report "clk= " & std_logic'image(clock)
& " reset= " & std_logic'image(reset)
& " start= " & std_logic'image(start)
& " write= " & std_logic'image(write_vscpu)
& " PC= " & integer'image(to_integer(unsigned(PC_ff)))
& " cstate= " & integer'image(to_integer(unsigned(stvar_ff)))
& " ac= " & integer'image(to_integer(unsigned(AC_ff)))
& " IR= " & integer'image(to_integer(unsigned(IR_ff)))
& " DR= " & integer'image(to_integer(unsigned(DR_ff)))
& " AR= " & integer'image(to_integer(unsigned(AR_ff)))
& " data_rd= " & integer'image(to_integer(unsigned(data_rd)))
& " read= " & std_logic'image(read_vscpu) 
& " address= " & integer'image(to_integer(unsigned(address)));
 
 end process;
end vscpu_arch;

