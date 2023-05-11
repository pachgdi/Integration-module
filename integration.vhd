---------------------------------------------------------------------------------- 
-- Company: WashU APT Collaboration
-- Engineer: Diana Pacheco-Garcia
-- 
-- Create Date: 04/12/2023 10:50:50 AM
-- Design Name: 
-- Module Name: integration - Behavioral

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_arith.ALL;
use IEEE.std_logic_signed.ALL; --  want them signed 


entity integration is
    Port ( clk             : IN STD_LOGIC;
           base_adr        : IN STD_LOGIC_VECTOR(7 downto 0); 
           oe              : IN STD_LOGIC;
           wea_fifo        : IN STD_LOGIC; 
           done_int        : OUT STD_LOGIC;
           peds_done       : OUT STD_LOGIC; 
           write_ints      : OUT STD_LOGIC;
           d               : OUT STD_LOGIC_VECTOR (31 downto 0);
           en_int          : IN STD_LOGIC );
          
end integration;

architecture Behavioral of integration is
    TYPE OUTPUTPEDSUBS IS ARRAY(4095 DOWNTO 0) of STD_LOGIC_VECTOR(15 DOWNTO 0);
    TYPE OUTPUTINTEGRALS IS ARRAY(63 DOWNTO 0) of STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL outputIntegralArray: OUTPUTINTEGRALS := (OTHERS => (OTHERS => '0')); 
    SIGNAL pedSubArray                          : OUTPUTPEDSUBS := (OTHERS => (OTHERS => '0'));
    SIGNAL pedSub_val                           : STD_LOGIC_VECTOR (15 downto 0);
    CONSTANT numSamples                         : STD_LOGIC_VECTOR(9 DOWNTO 0) := "0100000000";
    SIGNAL start                                : STD_LOGIC_VECTOR(9 DOWNTO 0):= "0000000000"; 
    SIGNAL endd                                 : STD_LOGIC_VECTOR(9 DOWNTO 0):= "0000000000";
    SIGNAL linear                               : STD_LOGIC; 
    SIGNAL current_integral                     : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL i_gte_start                          : STD_LOGIC := '0';
    SIGNAL l_lte_end                            : STD_LOGIC := '0'; 
    SIGNAL samplesCounter                       : STD_LOGIC_VECTOR (9 DOWNTO 0):= "0000000000"; 
    SIGNAL channelCounter                       : STD_LOGIC_VECTOR (4 DOWNTO 0) := "00000"; 
    signal addra                                : STD_LOGIC_VECTOR(11 DOWNTO 0) := "000000000000";
    CONSTANT start1                             : STD_LOGIC_VECTOR(9 DOWNTO 0):= "1111111011"; 
    CONSTANT end1                               : STD_LOGIC_VECTOR(9 DOWNTO 0):= "0000000101";
    CONSTANT start2                             : STD_LOGIC_VECTOR(9 DOWNTO 0):= "1111110110";
    CONSTANT end2                               : STD_LOGIC_VECTOR(9 DOWNTO 0):= "0000001010";
    CONSTANT start3                             : STD_LOGIC_VECTOR(9 DOWNTO 0):= "1111110001";
    CONSTANT end3                               : STD_LOGIC_VECTOR(9 DOWNTO 0):= "0000001111";
    CONSTANT start4                             : STD_LOGIC_VECTOR(9 DOWNTO 0):= "1111101100";
    CONSTANT end4                               : STD_LOGIC_VECTOR(9 DOWNTO 0):= "0000010100";
    SIGNAL start_temp                           : STD_LOGIC_VECTOR(9 DOWNTO 0) := "0000000000";--
    SIGNAL end_temp                             : STD_LOGIC_VECTOR(9 DOWNTO 0) := "0000000000";
    SIGNAL integralNum                          : INTEGER RANGE 0 TO 4 := 0;
    SIGNAL which_int                            : STD_LOGIC_VECTOR(1 DOWNTO 0) := "00"; -- number of integral
    SIGNAL myindex_ped                          : INTEGER RANGE 0 TO 4095 := 0; -- index for pedestal subtraction information
    SIGNAL myindex_intTemp                      : INTEGER RANGE 0 TO 63 := 0 ; -- index for integral values
    SIGNAL d_fifo                               : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL d_out_fifo                           : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL fifo_full                            : STD_LOGIC;--:= '0';
    SIGNAL fifo_oea                             : STD_LOGIC:='0'; --read from fifo
    SIGNAL fifo_empty                           :STD_LOGIC;--:='0';
    SIGNAL peds_read                            : STD_LOGIC:='0';
    SIGNAL fifo_counter                         : INTEGER:= 0;
    SIGNAL writing_ints                         : STD_LOGIC:='0'; -- signal set to 1 when writing to FIFO
    SIGNAL rd_latency                           : INTEGER := 0;
      
COMPONENT blk_mem_gen_0
PORT(clka: in STD_LOGIC;
    addra: in STD_LOGIC_VECTOR(11 DOWNTO 0);
    dina: in STD_LOGIC_VECTOR(15 DOWNTO 0);
    douta: out STD_LOGIC_VECTOR(15 DOWNTO 0);
    ena : in std_logic;
    wea: in STD_LOGIC_VECTOR(0 DOWNTO 0));
END COMPONENT;

COMPONENT fifo_generator_0
PORT(clk: STD_LOGIC;
    srst:  STD_LOGIC;
    full: out STD_LOGIC;
    din: in STD_LOGIC_VECTOR(31 DOWNTO 0);
    wr_en: in STD_LOGIC;
    empty: out STD_LOGIC;
    rd_en : in std_logic;
    dout: out STD_LOGIC_VECTOR(31 DOWNTO 0));
END COMPONENT;

begin

    myram:blk_mem_gen_0
    PORT MAP(clka => clk,
             addra => addra,
             dina => "0000000000000000",
             douta => pedSub_val,
             ena => oe,
             wea => "0");
             
    myfifo:fifo_generator_0
    PORT MAP(clk => clk,
             srst => '0',
             full => fifo_full,
             din => d_fifo,
             wr_en => wea_fifo,
             empty => fifo_empty,
             rd_en => fifo_oea, --oe
             dout => d_out_fifo); 

    d <= d_fifo WHEN (en_int='1' AND wea_fifo='1') ELSE "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";

    WITH which_int SELECT
        start_temp <= start1 WHEN "00",
                      start2 WHEN "01",
                      start3 WHEN "10",
                      start4 WHEN "11",
                     "0000000000" WHEN OTHERS;
                     
    WITH which_int SELECT
        end_temp <= end1 WHEN "00",
                      end2 WHEN "01",
                      end3 WHEN "10",
                      end4 WHEN "11",
                     "0000000000" WHEN OTHERS;

    -- initialize signals bound signals
    initprocess:PROCESS(clk) 
    begin
        IF(clk='1' AND clk'event) THEN
           IF( myindex_ped=4095) THEN 
               start <=  base_adr +start_temp; 
               endd <=  base_adr +end_temp; 
                IF(start < 0) THEN --
                    start <= start + numSamples - "000000001";
                END IF;
                IF(endd >= numSamples -1) THEN --
                    endd <= endd - (numSamples - "000000001");
                END IF;
                IF(endd >= start) THEN --
                    linear <= '1';
                ELSE
                    linear <= '0';
                END IF;
           END IF;
        END IF;  
    END PROCESS ;
    
    -- process for pedestal subtraction index counter 
    pedIndexCount:PROCESS(clk,peds_read) 
    BEGIN
        IF(clk='1' AND clk'event) THEN
            IF(peds_read = '0' AND oe='1' AND writing_ints='0') THEN
                IF(myindex_ped < 4095) THEN 
                    addra <= addra + 1;
                    IF(rd_latency = 1) THEN
                       myindex_ped <= myindex_ped + 1;
                    ELSE 
                        myindex_ped <= 0;
                    END IF;
                    IF(addra = 1) THEN
                        rd_latency <= rd_latency +1;
                    ELSE 
                        rd_latency <= rd_latency;
                    END IF;        
                ELSE
                    myindex_ped <= 0;
                    addra <= "000000000000";
                    rd_latency <= 0;
                END IF;
             END IF;
             IF(peds_read='1' AND writing_ints='0') THEN
                IF(myindex_ped < 4095) THEN 
                    myindex_ped <= myindex_ped + 1;
                ELSE
                    myindex_ped <= 0;
                END IF;
             END IF;       
        END IF;
    END PROCESS;
    
    -- process to read pedestal subtraction values from BRAM
    readPedSubs:PROCESS(clk,oe, en_int,myindex_intTemp)
    BEGIN
         IF(clk='1' AND clk'event) THEN
            IF(oe='1' AND myindex_ped < 4096) THEN 
                peds_done <= '0';
                pedSubArray(myindex_ped) <=  pedSub_val;
            END IF;
            IF(myindex_ped = 4095 AND which_int=0) THEN --
                peds_done <= '1';
                peds_read <= '1';
            END IF;
            IF(fifo_counter =63) THEN 
                peds_done <= '0';
                peds_read <= '0';
            END IF;
         END IF;
    END PROCESS;
    
    -- tracks which integral is ocurring 
    changeIntegralNum: PROCESS(integralNum)
    BEGIN
        CASE integralNum IS
            WHEN 0 => which_int <= "00";
            WHEN 1 => which_int <= "01";
            WHEN 2 => which_int <= "10";
            WHEN 3 => which_int <= "11";
            WHEN 4 => which_int <= "00";
            WHEN OTHERS => which_int <= "00";
       END CASE;
            
    END PROCESS ;
    
    -- process to keep counter of the index counter for the integral values, then triggers for the values to be written to FIFO
    intIndxCount: PROCESS(clk)
    BEGIN
        IF(clk='1' AND clk'event) THEN
            IF((samplesCounter = 255) AND peds_read='1') THEN 
                IF(myindex_intTemp = 63) THEN 
                    write_ints <= '1';
                    writing_ints <='1';
                    myindex_intTemp <= 0; 
                ELSE 
                    myindex_intTemp <= myindex_intTemp + 1;
                    write_ints <= '0';
                    writing_ints <='0';
                END IF;
             END IF;
       END IF;
    END PROCESS;
    
  -- process for channel counter 
    channelCount:PROCESS(clk)
    BEGIN
         IF(clk='1' AND clk'event) THEN
            IF((samplesCounter = 255) AND peds_read='1') THEN 
                IF(channelCounter = 15) THEN
                    channelCounter <= "00000";
                    integralNum <= integralNum + 1;
                ELSE 
                    channelCounter <= channelCounter +1;
                END IF;
            END IF;
        END IF;
    END PROCESS;
    
    -- process for sample counter 
    sampleCount:PROCESS(clk)
    BEGIN
         IF(clk='1' AND clk'event ) THEN 
            IF(peds_read='1' AND writing_ints='0') THEN
                IF(samplesCounter = 255) THEN 
                    samplesCounter <= "0000000000";
                ELSE
                    samplesCounter <= samplesCounter +1;
                END IF;
            END IF;
         END IF;
    END PROCESS;
    
    -- process for bounds in integration function
    bounds:PROCESS(clk,samplesCounter,peds_read, start, endd)
    BEGIN 
        IF(clk='1' AND clk'event) THEN
            IF(peds_read='1' OR (peds_read='0' AND myindex_ped=4095)) THEN
                IF(samplesCounter > (start-1)) THEN
                    i_gte_start <= '1';
                ELSE 
                    i_gte_start <= '0';
                END IF; 
                IF(samplesCounter < (endd +1)) THEN 
                    l_lte_end <= '1';
                ELSE 
                    l_lte_end <= '0';
                END IF; 
             END IF; 
         END IF; 
    END PROCESS;
            
    -- process of integration function
    integral:PROCESS(clk) 
    BEGIN
        IF(clk='1' AND clk'event) THEN
            IF( peds_read='1' AND writing_ints='0') THEN 
                    IF(samplesCounter > 0) THEN 
                        current_integral<= outputIntegralArray(myindex_intTemp);
                    ELSE
                        current_integral <= "00000000000000000000000000000000";
                    END IF;
                    IF((i_gte_start='1' AND l_lte_end='1') OR ((linear='0') AND (i_gte_start='1' OR l_lte_end='1'))) THEN
                        outputIntegralArray(myindex_intTemp)<= current_integral + pedSubArray(myindex_ped);
                    ELSE
                        outputIntegralArray(myindex_intTemp)<= current_integral;
                    END IF;
            END IF;
        END IF;
    END PROCESS ;

-- process to write integral values to FIFO 
    writeFifo: PROCESS(clk, wea_fifo,fifo_full)
    BEGIN
        IF(clk='1' AND clk'event) THEN
            IF(fifo_full = '0' AND wea_fifo='1' AND fifo_counter < 64) THEN
                d_fifo(31 DOWNTO 0) <= outputIntegralArray(fifo_counter);
                fifo_counter <= fifo_counter +1;
                done_int <='0';
            END IF;  
            IF (fifo_full='1') THEN
                done_int <='1';
                fifo_counter <= 0;
            END IF;         
       END IF;
    END PROCESS;
    

end Behavioral;

