-- BRIEF    : Component of FFT block
-- DATE     : March 23th, 2015
-- AUTHOR   : O.D

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

PACKAGE fft_radix2_dit_cmp IS

    COMPONENT fft_radix2_dit IS 
    
        GENERIC
        (
            G_NUM_STAGES    : POSITIVE    := 4; --Number of points = 2^G_NUM_STAGES
            G_CLIP          : BOOLEAN     := TRUE;
            G_LENGTH_BITS   : POSITIVE    := 4;
            G_SHIFT         : POSITIVE    := 8;
            G_BITS_IN       : POSITIVE    := 16;
            G_BITS_OUT      : POSITIVE    := 16
        );

        PORT 
        (
            P_clk_i         : IN STD_LOGIC;    -- CLK in
            P_rst_n_i       : IN STD_LOGIC;    -- RESET in (active at low level)
            P_ifft_i        : IN STD_LOGIC;
            P_start_i       : IN STD_LOGIC;
            P_data_re_i     : IN STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);
            P_data_im_i     : IN STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);
            P_atten_i       : IN STD_LOGIC_VECTOR(1 DOWNTO 0); -- Divide by 2^P_atten_i each butterfly
            P_busy_o        : OUT STD_LOGIC;
            P_data_re_o     : OUT STD_LOGIC_VECTOR(G_BITS_OUT - 1 DOWNTO 0);
            P_data_im_o     : OUT STD_LOGIC_VECTOR(G_BITS_OUT - 1 DOWNTO 0);
            P_done_o        : OUT STD_LOGIC
        );
    
    END COMPONENT;

END PACKAGE fft_radix2_dit_cmp;
