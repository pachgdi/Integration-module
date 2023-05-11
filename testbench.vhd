---------------------------------------------------------------------------------- 
-- Company: WashU APT Collaboration
-- Engineer: Diana Pacheco-Garcia
-- 
-- Create Date: 
-- Design Name: FPGA design of APT  
-- Module Name: testbench - Behavioral
-- Project Name: FPGA design of APT 
-- Target Devices: FPGA module with AMD Kintex 7 325T-2IF, 32MByte QSPI Flash 4x5cm 
-- Tool Versions: 
-- Description: FPGA design to implement the pedestal subtraction and integration functions of the computational pipeline 

----------------------------------------------------------------------------------
library IEEE;

use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_arith.ALL;
use IEEE.std_logic_signed.ALL;

entity testbench is
    --Port ( );
end testbench;

architecture Behavioral of testbench is

    TYPE states IS(s0,s1,s2,s3);
    SIGNAL state: states                              := s0;
    SIGNAL nxt_state: states                          := s0;
    signal reset_l    : STD_LOGIC                     := '0';
    signal base_adr   : STD_LOGIC_VECTOR(7 downto 0)  := "00010000"; -- would be an in signal from ped sub 
    SIGNAL clk        : STD_LOGIC                     := '0';
    SIGNAL sram_ena_l : STD_LOGIC;
    SIGNAL sram_wea_l : STD_LOGIC;
    SIGNAL inc        : STD_LOGIC;
    SIGNAL address    : STD_LOGIC_VECTOR(31 DOWNTO 0) := "00000000000000000000000000000000";
    SIGNAL d         : STD_LOGIC_VECTOR(31 DOWNTO 0)  := "00000000000000000000000000000000";
    SIGNAL done_int  : STD_LOGIC;
    SIGNAL read      : STD_LOGIC                     := '0'; -- READ PED SUBS
    SIGNAL write_ints: STD_LOGIC;
    SIGNAL write     : STD_LOGIC                     := '0';-- WRITE INTEGRALS 
    SIGNAL done      : STD_LOGIC                     := '0';
    SIGNAL int_ena   : STD_LOGIC                     := '0';
    SIGNAL int_oea   : STD_LOGIC                     := '0';
    SIGNAL int_wea   : STD_LOGIC                     := '0';
    SIGNAL peds_done : STD_LOGIC                     :='0';
 
    COMPONENT sram -- for testing purposes
    PORT (d       : IN STD_LOGIC_VECTOR(31 DOWNTO 0) ;
         ce_l     : IN    STD_LOGIC ;
         we_l     : IN    STD_LOGIC ;
         clk      : IN    STD_LOGIC) ;
    END COMPONENT;
    
    COMPONENT integration
    PORT( clk           : IN STD_LOGIC;
        base_adr        : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        oe              : IN STD_LOGIC;
        en_int          : IN STD_LOGIC;
        wea_fifo        : IN STD_LOGIC;
        done_int        : OUT STD_LOGIC;
        write_ints      : OUT STD_LOGIC;
        peds_done       : OUT STD_LOGIC;
        d               : OUT STD_LOGIC_VECTOR(31 DOWNTO 0) ); 
    END COMPONENT;

begin 
    sram_ena_l <= '0' WHEN (address >= 4100 AND (write='1')) ELSE '1'; 
    sram_wea_l <= '0' WHEN write='1' ELSE '1';
    clk        <= not clk after 5ns; -- 
    
    sram0:sram
    PORT MAP(clk    => clk,
            d       => d,
            ce_l    => sram_ena_l,
            we_l    => sram_wea_l);


    int_ena <= '1' WHEN address(31 DOWNTO 13) = "0000000000000000000" ELSE '0'; --- 
    int_wea <= '1' WHEN write ='1' ELSE '0'; 
    int_oea <= '1' WHEN read='1' ELSE '0';
    done <= '1'    WHEN done_int='1' ELSE '0';

    
    integration0:integration
    PORT MAP(clk             => clk,
            base_adr         => base_adr,
            oe               => int_oea,
            en_int           => int_ena,
            wea_fifo         => int_wea,
            done_int         => done_int,
            peds_done        => peds_done,
            write_ints       => write_ints,
            d                => d);
            
    -- clock process to  implement the state register 
    clkd: PROCESS(clk)
    BEGIN
        if (clk'EVENT AND clk='1') THEN
            IF( reset_l = '0') THEN
                state <= s0;
            else 
                state <= nxt_state;
            END IF;
        END IF;
   END PROCESS clkd;
   
   
   -- process to determine next states
   state_trans: PROCESS(state, int_ena,write_ints,done_int, peds_done)
   BEGIN 
        nxt_state <= state; 
        CASE state IS
            WHEN s0 => IF(int_ena='1') THEN
                            nxt_state <= s1;
                       ELSE
                            nxt_state <= s0;
                       END IF;
            WHEN s1 => IF(peds_done = '1') THEN 
                            nxt_state <= s2;
                       ELSE 
                            nxt_state <= s1; 
                       END IF; 
           WHEN s2 => IF(write_ints ='1') THEN 
                           nxt_state <= s3;
                       ELSE 
                            nxt_state <= s2;
                       END IF;
           WHEN s3 => IF(done_int ='1') THEN 
                            nxt_state <= s0;
                       ELSE 
                            nxt_state <= s3;
                       END IF;
        END CASE; 
   END PROCESS state_trans;
   
   inc <= '1' WHEN state=s3 or state=s1 ELSE '0';
   
   -- write process: integral values written to FIFO or to SRAM after state S3
   write_process: PROCESS(write_ints,done_int)
   BEGIN
        IF(write_ints='1' or done_int='1') THEN
            write <= '1';
        ELSE 
            write<='0';
        END IF;
   END PROCESS;
   
   -- read process: to read pedestal subtraction values from memory
   read_process: PROCESS(int_ena,peds_done,state)
   BEGIN
        IF(int_ena='1' AND peds_done/='1' AND state=s1) THEN
            read <= '1';
        ELSE 
            read <='0';
        END IF;
   END PROCESS;

   -- address counter for memory map of integration module 
   counteraddr: PROCESS(clk,inc)
   BEGIN
    IF(clk='1' AND clk'event) THEN
        IF(inc='1' AND address < 4164) THEN
            address <= address +1;
        ELSIF(address = 4164) THEN
            address <= "00000000000000000000000000000000";
        ELSE 
            address <= address;
        END IF;
     END IF;
   END PROCESS;
            
end Behavioral;

