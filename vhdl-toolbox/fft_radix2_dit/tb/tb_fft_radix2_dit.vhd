-- BRIEF   : Test Bench of FFT block
-- DATE    : March 23th, 2015
-- AUTHOR  : O.D

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

USE STD.TEXTIO.ALL;
USE IEEE.STD_LOGIC_TEXTIO.ALL;

LIBRARY WORK;
USE WORK.proj_config.ALL;
USE WORK.generic_fct.ALL;
USE WORK.fft_radix2_dit_cmp.ALL;
USE WORK.ram_cmp.ALL;
USE WORK.sin_cos_rom_cmp.ALL;

-- Empty entity for TB
ENTITY tb_fft_radix2_dit IS 
END tb_fft_radix2_dit;


-- TB architecture
ARCHITECTURE rtl OF tb_fft_radix2_dit IS 

-- Constants
CONSTANT G_NUM_STAGES   : POSITIVE  := C_MEM_POWER_2_DEPTH; --Number of points = 2^G_NUM_STAGES
CONSTANT G_CLIP         : BOOLEAN   := TRUE;
CONSTANT G_LENGTH_BITS  : POSITIVE  := C_MEM_POWER_2_DEPTH;
CONSTANT G_SHIFT        : POSITIVE  := 8;
CONSTANT G_BITS_IN      : POSITIVE  := C_MEM_DATA_LENGTH;
CONSTANT G_BITS_OUT     : POSITIVE  := 16;

-- Inputs
SIGNAL P_clk_i      : STD_LOGIC := '0'; -- CLK in
SIGNAL P_rst_n_i    : STD_LOGIC; -- RESET in (active at low level)
SIGNAL P_ifft_i     : STD_LOGIC := '0'; -- FFT
SIGNAL P_start_i    : STD_LOGIC;
SIGNAL P_data_re_i  : STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);
SIGNAL P_data_im_i  : STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);
SIGNAL P_atten_i    : STD_LOGIC_VECTOR(1 DOWNTO 0) := "01"; -- Divide by 2^P_atten_i each butterfly

-- Outputs
SIGNAL P_busy_o     : STD_LOGIC;
SIGNAL P_data_re_o  : STD_LOGIC_VECTOR(G_BITS_OUT - 1 DOWNTO 0);
SIGNAL P_data_im_o  : STD_LOGIC_VECTOR(G_BITS_OUT - 1 DOWNTO 0);
SIGNAL P_done_o     : STD_LOGIC;

-- Clock period definitions
SIGNAL stop             : BOOLEAN     := FALSE;
CONSTANT frequency      : INTEGER     := 100E6;
CONSTANT clk_period     : TIME        := 1 sec / frequency;

-- Text files
FILE file_samp_real : TEXT OPEN read_mode     IS "time_signal_real.txt";
FILE file_samp_imag : TEXT OPEN read_mode     IS "time_signal_imag.txt";
FILE fft_vhdl       : TEXT OPEN write_mode    IS "fft_vhdl.txt";

-- Signals
SIGNAL s_std_done_prev          : STD_LOGIC;
SIGNAL s_std_done_curr          : STD_LOGIC;
SIGNAL s_bool_ready             : BOOLEAN;
SIGNAL s_bool_write_offset_0    : BOOLEAN;
SIGNAL s_bool_read_offset_0     : BOOLEAN;

SIGNAL s_sig_write_fft_ram_real         : SIGNED(G_BITS_IN - 1 DOWNTO 0);
SIGNAL s_sig_write_fft_ram_imag         : SIGNED(G_BITS_IN - 1 DOWNTO 0);
SIGNAL s_uns_read_adr_ram_idx           : UNSIGNED(G_LENGTH_BITS - 1 DOWNTO 0);
SIGNAL s_uns_write_adr_ram_idx          : UNSIGNED(G_LENGTH_BITS - 1 DOWNTO 0);    
SIGNAL s_std_we                         : STD_LOGIC;
SIGNAL s_std_re                         : STD_LOGIC;
SIGNAL s_stdlv_read_fft_ram_real        : STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);
SIGNAL s_stdlv_read_fft_ram_imag        : STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);

SIGNAL s_stdlv_sin                      : STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);
SIGNAL s_stdlv_cos                      : STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);
SIGNAL s_uns_read_adr_sin_cos_rom_idx   : UNSIGNED(G_LENGTH_BITS - 1 DOWNTO 0);
SIGNAL xn_cos                           : STD_LOGIC_VECTOR(G_BITS_IN-1 DOWNTO 0);
SIGNAL xn_sin                           : STD_LOGIC_VECTOR(G_BITS_IN-1 DOWNTO 0);
    
    
--------------------------------------------------------
BEGIN
--------------------------------------------------------

-- MAP
uut: fft_radix2_dit

    GENERIC MAP
    (
        G_NUM_STAGES     => G_NUM_STAGES,
        G_CLIP           => G_CLIP,
        G_LENGTH_BITS    => G_LENGTH_BITS,
        G_SHIFT          => G_SHIFT,
        G_BITS_IN        => G_BITS_IN,
        G_BITS_OUT       => G_BITS_OUT
    )

    PORT MAP 
    (    
        P_clk_i         => P_clk_i,    
        P_rst_n_i       => P_rst_n_i,        
        P_ifft_i        => P_ifft_i,
        P_start_i       => P_start_i,
        P_data_re_i     => P_data_re_i,
        P_data_im_i     => P_data_im_i,
        P_atten_i       => P_atten_i,
        P_busy_o        => P_busy_o,
        P_data_re_o     => P_data_re_o,
        P_data_im_o     => P_data_im_o,
        P_done_o        => P_done_o
    );    
    
ram_re_inst : ram

    GENERIC MAP
    (
        G_LENGTH_BITS        => G_LENGTH_BITS,
        G_DATA_INOUT_BITS    => G_BITS_IN
    )

    PORT MAP 
    (
        P_rst_n_i       => P_rst_n_i,
        P_clk_i         => P_clk_i,
        P_datain_i      => STD_LOGIC_VECTOR(s_sig_write_fft_ram_real),
        P_read_adr_i    => STD_LOGIC_VECTOR(s_uns_read_adr_ram_idx),
        P_write_adr_i   => STD_LOGIC_VECTOR(s_uns_write_adr_ram_idx),
        P_we_i          => s_std_we,
        P_re_i          => s_std_re,
        P_dataout_o     => s_stdlv_read_fft_ram_real
    );
    
ram_im_inst : ram

    GENERIC MAP
    (
        G_LENGTH_BITS        => G_LENGTH_BITS,
        G_DATA_INOUT_BITS    => G_BITS_IN
    )

    PORT MAP 
    (
        P_rst_n_i       => P_rst_n_i,
        P_clk_i         => P_clk_i,
        P_datain_i      => STD_LOGIC_VECTOR(s_sig_write_fft_ram_imag),
        P_read_adr_i    => STD_LOGIC_VECTOR(s_uns_read_adr_ram_idx),
        P_write_adr_i   => STD_LOGIC_VECTOR(s_uns_write_adr_ram_idx),
        P_we_i          => s_std_we,
        P_re_i          => s_std_re,
        P_dataout_o     => s_stdlv_read_fft_ram_imag
    );
    
sin_cos_rom_inst : sin_cos_rom

    GENERIC MAP
    (
        G_LENGTH_BITS     => G_LENGTH_BITS,
        G_DATA_OUT_BITS   => G_BITS_IN
    )

    PORT MAP
    (
        P_clk_i       => P_clk_i,
        P_re_i        => s_std_re,
        P_adr_i       => STD_LOGIC_VECTOR(s_uns_read_adr_sin_cos_rom_idx),
        P_sin_o       => s_stdlv_sin,
        P_cos_o       => s_stdlv_cos
    );

-- Clock
P_clk_i <= '0' WHEN stop ELSE NOT P_clk_i AFTER clk_period/2;
    
-- Reset    
rst: PROCESS IS

    BEGIN
    
        --P_rst_n_i <= '1';
        --WAIT FOR 5 ns;
        
        P_rst_n_i <= '0';
        WAIT FOR 50 ns;
        
        P_rst_n_i <= '1';
        WAIT;
        
END PROCESS rst;


stop_tb : PROCESS(P_rst_n_i, P_clk_i) IS

BEGIN

    IF(P_rst_n_i = '0') THEN
        
        s_std_done_prev <= '0';
        s_std_done_curr <= '0';
        
    ELSIF RISING_EDGE(P_clk_i) THEN
    
        s_std_done_curr <= P_done_o;
        s_std_done_prev <= s_std_done_curr;
        
        IF (s_std_done_curr = '0' AND s_std_done_prev = '1') THEN --Falling Edge    
            stop <= TRUE; -- Stop TB    
        END IF;
        
    END IF;
    
END PROCESS stop_tb;


read_file : PROCESS(P_rst_n_i, P_clk_i) IS

        VARIABLE LR_i : LINE;
        VARIABLE LR_q : LINE;
        VARIABLE Ii : INTEGER;
        VARIABLE Qi : INTEGER;
        
    BEGIN    
    
        IF (P_rst_n_i = '0') THEN
        
            s_sig_write_fft_ram_real    <= (OTHERS => '0');
            s_sig_write_fft_ram_imag    <= (OTHERS => '0');
            s_uns_write_adr_ram_idx     <= (OTHERS => '0');
            s_std_we                    <= '0';
            
            s_bool_ready                <= FALSE;
            s_bool_write_offset_0       <= FALSE;

        ELSIF RISING_EDGE(P_clk_i) THEN
        
            IF (NOT ENDFILE(file_samp_real)) THEN -- Read until the end of file
                    
                s_std_we <= '1';
                
                IF( (s_uns_write_adr_ram_idx = TO_UNSIGNED(0, s_uns_write_adr_ram_idx'LENGTH)) AND (s_bool_write_offset_0 = FALSE) ) THEN
                    s_uns_write_adr_ram_idx <= TO_UNSIGNED(0, s_uns_write_adr_ram_idx'LENGTH);
                    s_bool_write_offset_0    <= TRUE;
                ELSE
                    s_uns_write_adr_ram_idx <= s_uns_write_adr_ram_idx + 1;
                END IF;
                
                READLINE(file_samp_real, LR_i);
                READ(LR_i,Ii);
                
                s_sig_write_fft_ram_real <= TO_SIGNED(Ii,G_BITS_IN);
                 
                READLINE(file_samp_imag, LR_q);
                READ(LR_q,Qi);
                
                s_sig_write_fft_ram_imag <= TO_SIGNED(Qi,G_BITS_IN);
                    
            END IF;
            
            IF (s_uns_write_adr_ram_idx = TO_UNSIGNED((2**G_LENGTH_BITS) - 1, s_uns_write_adr_ram_idx'LENGTH)) THEN
             
                s_std_we         <= '0';
                s_bool_ready     <= TRUE;
                
            END IF;

        END IF;
        
END PROCESS read_file;

save_result: process(P_clk_i) IS
    
        VARIABLE LR : LINE;
        VARIABLE Ii, Qi : INTEGER;
        
    BEGIN
    
        IF RISING_EDGE(P_clk_i) THEN
            
            IF (P_done_o = '1') THEN
            
                Ii := TO_INTEGER(SIGNED(P_data_re_o));
                Qi := TO_INTEGER(SIGNED(P_data_im_o)); 
                
                WRITE (LR,Ii);
                WRITE (LR,' ');
                WRITE (LR,Qi);
                
                WRITELINE(fft_vhdl, LR);
                    
            END IF;
                
        END IF;
            
END PROCESS save_result; 


start_fft : PROCESS(P_rst_n_i, P_clk_i) IS

        VARIABLE v_i, v_q        : STD_LOGIC_VECTOR(G_BITS_IN-1 DOWNTO 0);
        
    BEGIN    
    
        IF (P_rst_n_i = '0') THEN
        
            s_uns_read_adr_ram_idx           <= (OTHERS => '0');
            s_std_re                         <= '0';
            s_uns_read_adr_sin_cos_rom_idx   <= (OTHERS => '0');
            
            P_start_i                     <= '0';
            --P_data_re_i                 <= (OTHERS => '0');
            --P_data_im_i                 <= (OTHERS => '0');
            
            s_bool_read_offset_0        <= FALSE;
            
            v_i                            := (OTHERS => '0');
            v_q                            := (OTHERS => '0');
            xn_cos                         <= (OTHERS => '0');
            xn_sin                         <= (OTHERS => '0');

        ELSIF RISING_EDGE(P_clk_i) THEN
        
            IF ( (s_bool_ready = TRUE) AND (s_uns_read_adr_ram_idx < TO_UNSIGNED((2**G_LENGTH_BITS) - 1, s_uns_read_adr_ram_idx'LENGTH)) ) THEN
                
                -- Increment RAM R address (test also ROM read)
                IF( (s_uns_read_adr_ram_idx = TO_UNSIGNED(0, s_uns_read_adr_ram_idx'LENGTH)) AND (s_bool_read_offset_0 = FALSE) ) THEN
                    s_uns_read_adr_ram_idx              <= TO_UNSIGNED(0, s_uns_read_adr_ram_idx'LENGTH);
                    s_uns_read_adr_sin_cos_rom_idx      <= TO_UNSIGNED(0, s_uns_read_adr_sin_cos_rom_idx'LENGTH);
                    s_bool_read_offset_0                <= TRUE;
                ELSE
                    s_uns_read_adr_ram_idx             <= s_uns_read_adr_ram_idx + 1;
                    s_uns_read_adr_sin_cos_rom_idx     <= s_uns_read_adr_sin_cos_rom_idx + 1;
                END IF;
                
                 -- Read samples from RAM
                s_std_re <= '1';
                
                -- Start FFT
                P_start_i <= '1'; -- FFT will start on RE of P_start_i
                
            ELSE 
            
                s_std_re     <= '0';
                P_start_i     <= '0';
                
            END IF;
            
            -- Test ROM read
            IF (s_uns_read_adr_sin_cos_rom_idx >= TO_UNSIGNED(1, s_uns_read_adr_sin_cos_rom_idx'LENGTH)) THEN
            
                v_i        := STD_LOGIC_VECTOR(SIGNED(s_stdlv_cos)); 
                xn_cos     <= v_i;
                v_q        := STD_LOGIC_VECTOR(SIGNED(s_stdlv_sin));
                xn_sin     <= v_q;
                
            END IF;
                
        END IF;
    
        
END PROCESS start_fft;

P_data_re_i <= s_stdlv_read_fft_ram_real;
P_data_im_i <= s_stdlv_read_fft_ram_imag;
    
END rtl;
