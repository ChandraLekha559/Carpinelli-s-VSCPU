library ieee ; 
use ieee.std_logic_1164.all ;

entity vscputb is end vscputb ;

architecture stim of vscputb is
component vscpu
generic( VSCPU_A_WIDTH : integer := 8 ;
			VSCPU_D_WIDTH : integer := 16);
port( clock, reset, start, write_vscpu : in std_logic;
		addr : in std_logic_vector(VSCPU_A_WIDTH-1 downto 0);
		data : in std_logic_vector(VSCPU_D_WIDTH-1 downto 0);
		status : inout std_logic);
end component; 
constant INSTR_add : std_logic_vector(3 downto 0) := "0000";
constant INSTR_and : std_logic_vector(3 downto 0) := "0001";
constant INSTR_jmp : std_logic_vector(3 downto 0) := "0010";
constant INSTR_inc : std_logic_vector(3 downto 0) := "0011";
constant INSTR_dcr : std_logic_vector(3 downto 0) := "0100";
constant INSTR_lda : std_logic_vector(3 downto 0) := "0101";
constant INSTR_sub : std_logic_vector(3 downto 0) := "0110";
constant INSTR_mul : std_logic_vector(3 downto 0) := "0111";
constant A_WIDTH : integer := 8 ; 
constant D_WIDTH : integer := 16 ;
  
signal clk , reset , start , write_en : std_logic ;
signal addr : std_logic_vector( A_WIDTH-1 downto 0 ) ; 
signal data : std_logic_vector ( D_WIDTH-1 downto 0 ) ;
signal status : std_logic ;

  procedure do_synch_active_high_half_pulse ( 
      signal formal_p_clk : in std_logic ; 
      signal formal_p_sig : out std_logic 
    ) is
  begin
    wait until formal_p_clk='0' ;  formal_p_sig <= '1' ;
    wait until formal_p_clk='1' ;  formal_p_sig <= '0' ;
  end procedure ;
  
  procedure do_program ( 
      signal formal_p_clk : in std_logic ; 
      signal formal_p_write_en : out std_logic ; 
      signal formal_p_addr_out , formal_p_data_out : out std_logic_vector ;
      formal_p_ADDRESS_in , formal_p_DATA_in : in std_logic_vector     
    ) is
  begin
    wait until formal_p_clk='0' ;  formal_p_write_en <= '1' ;
    formal_p_addr_out <= formal_p_ADDRESS_in ; 
    formal_p_data_out <= formal_p_DATA_in ;
    wait until formal_p_clk='1' ;  formal_p_write_en <='0' ;
  end do_program ;

begin    
   dut_vscpu : vscpu  port map ( clock => clk , reset => reset , start => start ,
             write_vscpu => write_en , addr => addr  , data => data , status => status) ;
             
  process begin
    clk <= '0' ;
    for i in 0 to 249 loop 
      wait for 1 ns ; clk <= '1' ;  wait for 1 ns ; clk <= '0';
    end loop ;
    wait ;
  end process ;

  
  process begin
    reset <= '0' ;  start <= '0' ; write_en <= '0' ;
    addr <= "00000000" ;  data <= "0000000000000000" ;
    do_synch_active_high_half_pulse ( clk, reset ) ; -- acc=0

    do_program ( clk, write_en, addr, data, "00000001" , INSTR_lda & "0000" & "00010100"  ) ;
    do_program ( clk, write_en, addr, data, "00000010" , INSTR_add & "0000" & "00010101"  ) ;
    do_program ( clk, write_en, addr, data, "00000011" , INSTR_and & "0000" & "00010110"  ) ;
    do_program ( clk, write_en, addr, data, "00000100" , INSTR_dcr & "0000" & "00000000"  ) ;
   
    do_program ( clk, write_en, addr, data, "00000101" , INSTR_jmp & "0000" & "00000100"  ) ;
    
    do_program ( clk, write_en, addr, data, "00000110" , INSTR_lda & "0000" & "00011000"  ) ;
    do_program ( clk, write_en, addr, data, "00000111" , INSTR_sub & "0000" & "00011001"  ) ;
    
    do_program ( clk, write_en, addr, data, "00001000" , INSTR_jmp & "0000" & "00001001"  ) ;
    
    do_program ( clk, write_en, addr, data, "00001001" , INSTR_inc & "0000" & "00000000"  ) ;
    
    do_program ( clk, write_en, addr, data, "00010100" , "0000000000000010"  ) ; --1st load
    do_program ( clk, write_en, addr, data, "00010101" , "0000000110010010"  ) ; --add
    do_program ( clk, write_en, addr, data, "00010110" , "0000001001100100"  ) ; --and
    
    do_program ( clk, write_en, addr, data, "00011000" , "0000000000000110"  ) ; --2nd load
    do_program ( clk, write_en, addr, data, "00011001" , "0000000000000100"  ) ; --sub
    
    do_synch_active_high_half_pulse ( clk, start ) ;
    wait ;
  end process ;
end stim ;