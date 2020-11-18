-- BRIEF    : Component of RAM block
-- DATE     : March 23th, 2015
-- AUTHOR   : O.D

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

PACKAGE ram_cmp IS

    COMPONENT ram IS 
    
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
    
    END COMPONENT;

END PACKAGE ram_cmp;
