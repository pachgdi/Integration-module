---------------------------------------------------------------------------------- 
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04/12/2023 01:53:28 PM
-- Design Name: 
-- Module Name: sram - Behavioral uses code from WDR 362 class 
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: write integral values to SRAM 
-- 

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL ;
USE IEEE.STD_LOGIC_ARITH.ALL ;

ENTITY sram IS
   PORT (d        : IN STD_LOGIC_VECTOR(31 DOWNTO 0) ;
         ce_l     : IN    STD_LOGIC ;
         we_l     : IN    STD_LOGIC ;
         clk      : IN    STD_LOGIC) ;
END sram ;

ARCHITECTURE behavioral OF sram IS

   TYPE memoryarray IS ARRAY(63 DOWNTO 0) OF STD_LOGIC_VECTOR(31 DOWNTO 0) ;

   SIGNAL myarray : memoryarray := (OTHERS => (OTHERS => '0')) ;

   SIGNAL myindex : INTEGER RANGE 0 TO 63 := 0 ;


BEGIN

   writeprocess:PROCESS(clk)
   begin
      IF (clk = '1' AND clk'event) THEN
         IF (ce_l = '0' AND we_l = '0' AND myindex < 64) THEN
            myarray(myindex) <= d(31 DOWNTO 0); 
            myindex <= myindex +1;
         END IF;
      END IF;
   END PROCESS writeprocess ;


END behavioral ;

