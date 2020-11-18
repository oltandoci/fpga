-- BRIEF    : SIN/COS FFT coeffs
-- DATE     : March 23th, 2015
-- AUTHOR   : O.D

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

LIBRARY WORK;
USE WORK.proj_config.ALL;
USE WORK.generic_fct.ALL;

ENTITY sin_cos_rom IS 

    GENERIC
    (    
        G_LENGTH_BITS     : POSITIVE := C_MEM_POWER_2_DEPTH; --4;
        G_DATA_OUT_BITS   : POSITIVE := C_MEM_DATA_LENGTH --16
    );

    PORT 
    (
        P_clk_i       : IN STD_LOGIC;
        P_re_i        : IN STD_LOGIC;
        P_adr_i       : IN STD_LOGIC_VECTOR(G_LENGTH_BITS - 1 DOWNTO 0);
        P_sin_o       : OUT STD_LOGIC_VECTOR(G_DATA_OUT_BITS - 1 DOWNTO 0);
        P_cos_o       : OUT STD_LOGIC_VECTOR(G_DATA_OUT_BITS - 1 DOWNTO 0)
    );
    
END sin_cos_rom;

ARCHITECTURE rtl OF sin_cos_rom IS

CONSTANT c_rom_sin : t_sin_cos_table := FCT_init_sin_table;
CONSTANT c_rom_cos : t_sin_cos_table := FCT_init_cos_table;

-- TYPE mem_type IS ARRAY (0 TO ((2**G_LENGTH_BITS) - 1)) OF STD_LOGIC_VECTOR(G_DATA_OUT_BITS - 1 DOWNTO 0);
    
-- CONSTANT c_rom_sin : mem_type := ( 

-- 0 => STD_LOGIC_VECTOR(TO_SIGNED(0, 16)),
-- 1 => STD_LOGIC_VECTOR(TO_SIGNED(0, 16)),
-- 2 => STD_LOGIC_VECTOR(TO_SIGNED(-32765, 16)),
-- 3 => STD_LOGIC_VECTOR(TO_SIGNED(0, 16)),
-- 4 => STD_LOGIC_VECTOR(TO_SIGNED(-23168, 16)),
-- 5 => STD_LOGIC_VECTOR(TO_SIGNED(-32765, 16)),
-- 6 => STD_LOGIC_VECTOR(TO_SIGNED(-23168, 16)),
-- 7 => STD_LOGIC_VECTOR(TO_SIGNED(0, 16)),
-- 8 => STD_LOGIC_VECTOR(TO_SIGNED(-12539, 16)),
-- 9 => STD_LOGIC_VECTOR(TO_SIGNED(-23168, 16)),
-- 10 => STD_LOGIC_VECTOR(TO_SIGNED(-30271, 16)),
-- 11 => STD_LOGIC_VECTOR(TO_SIGNED(-32765, 16)),
-- 12 => STD_LOGIC_VECTOR(TO_SIGNED(-30271, 16)),
-- 13 => STD_LOGIC_VECTOR(TO_SIGNED(-23168, 16)),
-- 14 => STD_LOGIC_VECTOR(TO_SIGNED(-12539, 16)),
-- 15 => STD_LOGIC_VECTOR(TO_SIGNED(0, 16)) );


-- CONSTANT c_rom_cos : mem_type := ( 

-- 0 => STD_LOGIC_VECTOR(TO_SIGNED(32765, 16)),
-- 1 => STD_LOGIC_VECTOR(TO_SIGNED(32765, 16)),
-- 2 => STD_LOGIC_VECTOR(TO_SIGNED(0, 16)),
-- 3 => STD_LOGIC_VECTOR(TO_SIGNED(32765, 16)),
-- 4 => STD_LOGIC_VECTOR(TO_SIGNED(23168, 16)),
-- 5 => STD_LOGIC_VECTOR(TO_SIGNED(0, 16)),
-- 6 => STD_LOGIC_VECTOR(TO_SIGNED(-23168, 16)),
-- 7 => STD_LOGIC_VECTOR(TO_SIGNED(32765, 16)),
-- 8 => STD_LOGIC_VECTOR(TO_SIGNED(30271, 16)),
-- 9 => STD_LOGIC_VECTOR(TO_SIGNED(23168, 16)),
-- 10 => STD_LOGIC_VECTOR(TO_SIGNED(12539, 16)),
-- 11 => STD_LOGIC_VECTOR(TO_SIGNED(0, 16)),
-- 12 => STD_LOGIC_VECTOR(TO_SIGNED(-12539, 16)),
-- 13 => STD_LOGIC_VECTOR(TO_SIGNED(-23168, 16)),
-- 14 => STD_LOGIC_VECTOR(TO_SIGNED(-30271, 16)),
-- 15 => STD_LOGIC_VECTOR(TO_SIGNED(0, 16)) );

------------------------------------------ 
BEGIN 
------------------------------------------ 

    read_rom: PROCESS (P_clk_i)

        BEGIN
        
            IF RISING_EDGE(P_clk_i) THEN
            
                IF (P_re_i = '1') THEN
                
                    P_sin_o <= c_rom_sin(TO_INTEGER(UNSIGNED(P_adr_i)));
                    P_cos_o <= c_rom_cos(TO_INTEGER(UNSIGNED(P_adr_i)));
                    
                END IF;
                
            END IF;
        
    END PROCESS read_rom;
    
END rtl;
