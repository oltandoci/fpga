-- BRIEF    : Component of SIN_COS_ROM block
-- DATE     : March 23th, 2015
-- AUTHOR   : O.D

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

PACKAGE sin_cos_rom_cmp IS

    COMPONENT sin_cos_rom IS 
    
        GENERIC
        (    
            G_LENGTH_BITS     : POSITIVE := 4;
            G_DATA_OUT_BITS   : POSITIVE := 16
        );

        PORT 
        (
            P_clk_i	    : IN STD_LOGIC;
            P_re_i      : IN STD_LOGIC;
            P_adr_i     : IN STD_LOGIC_VECTOR(G_LENGTH_BITS - 1 DOWNTO 0);
            P_sin_o     : OUT STD_LOGIC_VECTOR(G_DATA_OUT_BITS - 1 DOWNTO 0);
            P_cos_o     : OUT STD_LOGIC_VECTOR(G_DATA_OUT_BITS - 1 DOWNTO 0)
        );
    
    END COMPONENT;

END PACKAGE sin_cos_rom_cmp;
