-- BRIEF    : RAM used for FFT (samples are sent by bursts at FFT block). 
--            The implementation should be done by block RAM (check option "BLOCK" in ISE).
--            WARNING : To synthetize this ram by blocks, the RESET signal must be SYNCHRONOUS.
-- DATE     : March 23th, 2015
-- AUTHOR   : O.D

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY ram IS 
    
    GENERIC
    (
        G_LENGTH_BITS        : POSITIVE := 8;
        G_DATA_INOUT_BITS    : POSITIVE := 16
    );

    PORT  
    (
        P_rst_n_i       : IN STD_LOGIC; -- RESET actif at low level
        P_clk_i         : IN STD_LOGIC; -- CLK input
        P_datain_i      : IN STD_LOGIC_VECTOR(G_DATA_INOUT_BITS - 1 DOWNTO 0);
        P_read_adr_i    : IN STD_LOGIC_VECTOR(G_LENGTH_BITS - 1 DOWNTO 0);
        P_write_adr_i   : IN STD_LOGIC_VECTOR(G_LENGTH_BITS - 1 DOWNTO 0);
        P_we_i          : IN STD_LOGIC;
        P_re_i          : IN STD_LOGIC;
        P_dataout_o     : OUT STD_LOGIC_VECTOR(G_DATA_INOUT_BITS - 1 DOWNTO 0)
    );
    
END ram;

ARCHITECTURE rtl OF ram IS

    TYPE mem_type IS ARRAY (0 TO ((2**G_LENGTH_BITS) - 1)) OF STD_LOGIC_VECTOR (G_DATA_INOUT_BITS - 1 DOWNTO 0);
    SIGNAL s_tab_mem : mem_type;      
      
---------------------------
BEGIN
---------------------------

    write_ram: PROCESS (P_clk_i) 
     
        BEGIN
        
            IF RISING_EDGE(P_clk_i) THEN
            
                IF (P_we_i = '1') THEN 
                    s_tab_mem(TO_INTEGER(UNSIGNED(P_write_adr_i))) <= P_datain_i;
                END IF;
                
            END IF;
            
    END PROCESS write_ram;

    read_ram: PROCESS (P_rst_n_i, P_clk_i) 

        BEGIN
        
            IF RISING_EDGE(P_clk_i) THEN
            
                --RESET synchronous
                IF (P_rst_n_i = '0') THEN
                    P_dataout_o <= (OTHERS => '0');
                END IF;
                
                IF (P_re_i = '1') THEN 
                    P_dataout_o <= s_tab_mem(TO_INTEGER(UNSIGNED(P_read_adr_i)));
                ELSE
                    P_dataout_o <= (OTHERS => '0');
                END IF;
                
            END IF;
        
    END PROCESS read_ram;
  
END rtl;
