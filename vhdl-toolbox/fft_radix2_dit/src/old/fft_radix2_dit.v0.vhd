-- BRIEF        : Implementation of FFT/IFFT with :
--                - Radix 2
--                - Number of points must be power of 2
--                - Cooley-Tukey butterfly using decimation in time (DIT)    
--                The number of points is deduced by 2^G_NUM_STAGES
--                The input vector must be provided in bit reversed order
--                The output vector is sent out by burst and in natural order
--                The FFT starts on tick rising edge of "P_start_i"
--                At this point the signal "P_busy_o" is set to '1' during all FFT process
--                When FFT process is done, the output samples are sent out by burst and the signal "P_done_o" is set to '1'
--                In order to perform an IFFT, the signal "P_ifft_i" must be set to '1', otherwise a FFT is performed
--                The input vector is supposed to be complex with real and imaginary parts and it's the same for output saples
--                SIN/COS coefficients are quantified as signed "G_BITS_IN" bits (i.e. float sin/cos multiplied by 2^(G_BITS_IN - 1))
--                Because of multiplication, the "G_SHIFT" constant is used to shift result to right (i.e division by 2^G_SHIFT)
--                The "G_CLIP" option is applied at the butterfly multiplication in order to manage overflow
--                In case of an IFFT, the butterfly result is shifted by 2 at each stage, sat at the end the output is divided by 2^G_NUM_STAGES
--                The "P_atten_i" fom 0 to 3 is a shift for the butterfly result in order to prevent overflow
--                Here are described the above details for a butterfly operation :
--
--                T = [ ( W_sincos >> (G_BITS_IN - 1 - G_SHIFT) ) * X_half ] >> G_SHIFT : 2 16b signed operations so result is 32b signed
--                IF ( G_CLIP = TRUE ) : IF ( T > +2^(G_BITS_OUT - 1) - 1), in this case clip T to +2^(G_BITS_OUT - 1) - 1
--                IF ( G_CLIP = TRUE ) : IF ( T < -2^(G_BITS_OUT - 1) ), in this case clip T to -2^(G_BITS_OUT - 1)
--                X_half         = [ X_current - T ] >> P_atten_i
--                X_current     = [ X_current + T ] >> P_atten_i
--                IF ( P_ifft_i = '1' ) : X_half = X_half >> 1 AND and X_current = X_current >> 1
--                IF ( G_CLIP = TRUE ) : IF ( X_half OR X_current > +2^(G_BITS_OUT - 1) - 1), in this case clip X_half OR X_current to +2^(G_BITS_OUT - 1) - 1
--                IF ( G_CLIP = TRUE ) : IF ( X_half OR X_current < -2^(G_BITS_OUT - 1) ), in this case clip X_half OR X_current to -2^(G_BITS_OUT - 1)
--
--                For more details please see the concerned technical document    
-- DATE         : March 23th, 2015
-- AUTHOR       : O.D

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

LIBRARY WORK;
USE WORK.proj_config.ALL;
USE WORK.generic_fct.ALL;
USE WORK.ram_cmp.ALL;
USE WORK.sin_cos_rom_cmp.ALL;


ENTITY fft_radix2_dit IS 

    GENERIC
    (
        G_NUM_STAGES     : POSITIVE     := C_MEM_POWER_2_DEPTH; --Number of points = 2^G_NUM_STAGES
        G_CLIP             : BOOLEAN     := TRUE;
        G_LENGTH_BITS     : POSITIVE     := C_MEM_POWER_2_DEPTH;
        G_SHIFT         : POSITIVE     := 8;
        G_BITS_IN         : POSITIVE     := C_MEM_DATA_LENGTH;
        G_BITS_OUT         : POSITIVE     := 16
    );

    PORT 
    (
        P_clk_i         : IN STD_LOGIC;    -- CLK in
        P_rst_n_i         : IN STD_LOGIC;    -- RESET in (active at low level)
        P_ifft_i         : IN STD_LOGIC;
        P_start_i         : IN STD_LOGIC;
        P_data_re_i        : IN STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);
        P_data_im_i        : IN STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);
        P_atten_i        : IN STD_LOGIC_VECTOR(1 DOWNTO 0); -- Divide by 2^P_atten_i each butterfly
        P_busy_o          : OUT STD_LOGIC;
        P_data_re_o        : OUT STD_LOGIC_VECTOR(G_BITS_OUT - 1 DOWNTO 0);
        P_data_im_o        : OUT STD_LOGIC_VECTOR(G_BITS_OUT - 1 DOWNTO 0);
        P_done_o         : OUT STD_LOGIC
    );
    
END fft_radix2_dit;

ARCHITECTURE rtl OF fft_radix2_dit IS 

--CONSTANT C_RADIX_2    : INTEGER := 2;
CONSTANT C_NB_POINTS_MAX    : POSITIVE := 2**G_NUM_STAGES;
CONSTANT C_MEM_IDX_MAX         : POSITIVE := (2**G_LENGTH_BITS) - 1;
CONSTANT C_CLIP_MAX            : SIGNED(31 DOWNTO 0) := TO_SIGNED(+(2**G_BITS_OUT - 1) - 1, 32);
CONSTANT C_CLIP_MIN            : SIGNED(31 DOWNTO 0) := TO_SIGNED(-(2**G_BITS_OUT - 1), 32);

TYPE fft_status IS (status_fft_idle, 
                    status_fft_read_samples_bit_reverse_idx, 
                    status_fft_update_stage, 
                    status_fft_loop_butterfly_per_group, status_fft_perform_butterfly, 
                    status_fft_pipeline,
                    status_fft_done);
                    
SIGNAL sm_fft_status : fft_status;

TYPE butterfly_step IS (butterfly_pipeline_1,
                        butterfly_step_cplx_mult_a, butterfly_step_cplx_mult_b, 
                        butterfly_step_cplx_mult_c, butterfly_step_cplx_mult_d, 
                        butterfly_step_cplx_mult_res,
                        butterfly_pipeline_2,
                        butterfly_step_cplx_sum_half_read, butterfly_step_cplx_sum_half_write,
                        butterfly_pipeline_3,
                        butterfly_step_cplx_sum_curr_read, butterfly_step_cplx_sum_curr_write);
                        
SIGNAL sm_butterfly_step : butterfly_step;

--SIGNAL s_bool_write_offset_0    : BOOLEAN;
--SIGNAL s_bool_read_offset_0        : BOOLEAN;
SIGNAL s_uns_read_spectrum_idx    : UNSIGNED(G_LENGTH_BITS - 1 DOWNTO 0);

SIGNAL s_sig_write_fft_ram_real            : SIGNED(G_BITS_IN - 1 DOWNTO 0);
SIGNAL s_sig_write_fft_ram_imag            : SIGNED(G_BITS_IN - 1 DOWNTO 0);
SIGNAL s_uns_read_adr_sin_cos_rom_idx    : UNSIGNED(G_LENGTH_BITS - 1 DOWNTO 0);
SIGNAL s_uns_read_adr_ram_idx            : UNSIGNED(G_LENGTH_BITS - 1 DOWNTO 0);
SIGNAL s_uns_write_adr_ram_idx             : UNSIGNED(G_LENGTH_BITS - 1 DOWNTO 0);    
SIGNAL s_std_we                            : STD_LOGIC;
SIGNAL s_std_re                            : STD_LOGIC;
SIGNAL s_stdlv_read_fft_ram_real        : STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);
SIGNAL s_stdlv_read_fft_ram_imag        : STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);
SIGNAL s_stdlv_sin                        : STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);
SIGNAL s_stdlv_cos                        : STD_LOGIC_VECTOR(G_BITS_IN - 1 DOWNTO 0);


--------------------------------
BEGIN
--------------------------------

ram_re_inst : ram

    GENERIC MAP
    (
        G_LENGTH_BITS         => G_LENGTH_BITS,
        G_DATA_INOUT_BITS    => G_BITS_IN
    )

    PORT MAP 
    (
        P_rst_n_i        => P_rst_n_i,
        P_clk_i            => P_clk_i,
        P_datain_i        => STD_LOGIC_VECTOR(s_sig_write_fft_ram_real),
        P_read_adr_i    => STD_LOGIC_VECTOR(s_uns_read_adr_ram_idx),
        P_write_adr_i    => STD_LOGIC_VECTOR(s_uns_write_adr_ram_idx),
        P_we_i            => s_std_we,
        P_re_i            => s_std_re,
        P_dataout_o     => s_stdlv_read_fft_ram_real
    );
    
ram_im_inst : ram

    GENERIC MAP
    (
        G_LENGTH_BITS         => G_LENGTH_BITS,
        G_DATA_INOUT_BITS    => G_BITS_IN
    )

    PORT MAP 
    (
        P_rst_n_i        => P_rst_n_i,
        P_clk_i            => P_clk_i,
        P_datain_i        => STD_LOGIC_VECTOR(s_sig_write_fft_ram_imag),
        P_read_adr_i    => STD_LOGIC_VECTOR(s_uns_read_adr_ram_idx),
        P_write_adr_i    => STD_LOGIC_VECTOR(s_uns_write_adr_ram_idx),
        P_we_i            => s_std_we,
        P_re_i            => s_std_re,
        P_dataout_o     => s_stdlv_read_fft_ram_imag
    );
    
sin_cos_rom_inst : sin_cos_rom

    GENERIC MAP
    (
        G_LENGTH_BITS     => G_LENGTH_BITS,
        G_DATA_OUT_BITS    => G_BITS_IN
    )

    PORT MAP
    (
        P_clk_i        => P_clk_i,
        P_re_i        => s_std_re,
        P_adr_i        => STD_LOGIC_VECTOR(s_uns_read_adr_sin_cos_rom_idx),
        P_sin_o        => s_stdlv_sin,
        P_cos_o        => s_stdlv_cos
    );


    proc_main : PROCESS(P_rst_n_i, P_clk_i) IS
    
        VARIABLE v_temp_re            : SIGNED(31 DOWNTO 0);
        VARIABLE v_temp_im            : SIGNED(31 DOWNTO 0);
        VARIABLE v_temp_a             : SIGNED(31 DOWNTO 0);
        VARIABLE v_temp_b             : SIGNED(31 DOWNTO 0);
        VARIABLE v_temp_c             : SIGNED(31 DOWNTO 0);
        VARIABLE v_temp_d             : SIGNED(31 DOWNTO 0);
        VARIABLE v_int_atten        : INTEGER;
        VARIABLE v_shift_re            : SIGNED(31 DOWNTO 0);
        VARIABLE v_shift_im            : SIGNED(31 DOWNTO 0);
        VARIABLE v_scale_re_ifft    : SIGNED(31 DOWNTO 0);
        VARIABLE v_scale_im_ifft    : SIGNED(31 DOWNTO 0);
        VARIABLE v_uns_temp_adr        : UNSIGNED(G_LENGTH_BITS - 1 DOWNTO 0);
        VARIABLE v_uns_cpt_adr        : UNSIGNED(G_LENGTH_BITS - 1 DOWNTO 0);
        VARIABLE v_uns_stage         : UNSIGNED(G_LENGTH_BITS + 1 DOWNTO 0);
        VARIABLE v_uns_points         : UNSIGNED(G_LENGTH_BITS + 1 DOWNTO 0);
        VARIABLE v_uns_butterfly     : UNSIGNED(G_LENGTH_BITS + 1 DOWNTO 0);
        VARIABLE v_uns_loop_grp     : UNSIGNED(G_LENGTH_BITS + 1 DOWNTO 0);
        VARIABLE v_uns_index_but    : UNSIGNED(G_LENGTH_BITS + 1 DOWNTO 0);
        VARIABLE v_uns_half_index    : UNSIGNED(G_LENGTH_BITS + 1 DOWNTO 0);
        
    BEGIN
    
    IF (P_rst_n_i = '0') THEN
    
        -- ARCHITECTURE signals
        sm_fft_status            <= status_fft_idle;
        sm_butterfly_step        <= butterfly_pipeline_1;    
        --s_bool_write_offset_0    <= FALSE;
        --s_bool_read_offset_0    <= FALSE;
        s_uns_read_spectrum_idx    <= (OTHERS => '1');
        
        -- MAP signals
        s_sig_write_fft_ram_real        <= (OTHERS => '0');
        s_sig_write_fft_ram_imag        <= (OTHERS => '0');
        s_uns_read_adr_sin_cos_rom_idx    <= (OTHERS => '0');
        s_uns_read_adr_ram_idx             <= (OTHERS => '0');
        s_uns_write_adr_ram_idx            <= (OTHERS => '0');
        s_std_we                        <= '0';
        s_std_re                        <= '0';
        
        -- ENTITY out signals
        P_busy_o          <= '0';
        P_data_re_o        <= (OTHERS => '0');
        P_data_im_o        <= (OTHERS => '0');
        P_done_o         <= '0';
        
        -- PROCESS local variables
        v_temp_re             := (OTHERS => '0');
        v_temp_im             := (OTHERS => '0');
        v_temp_a             := (OTHERS => '0');
        v_temp_b             := (OTHERS => '0');
        v_temp_c             := (OTHERS => '0');
        v_temp_d             := (OTHERS => '0');
        v_int_atten            := 0;
        v_shift_re            := (OTHERS => '0');
        v_shift_im            := (OTHERS => '0');
        v_scale_re_ifft        := (OTHERS => '0');
        v_scale_im_ifft        := (OTHERS => '0');
        v_uns_temp_adr        := (OTHERS => '0');
        v_uns_cpt_adr        := (OTHERS => '0');
        v_uns_stage         := TO_UNSIGNED(1, v_uns_stage'LENGTH); --TO_UNSIGNED(C_RADIX_2, v_uns_stage'LENGTH);
        v_uns_points        := (OTHERS => '0');
        v_uns_butterfly     := (OTHERS => '0');
        v_uns_loop_grp        := (OTHERS => '0');
        v_uns_index_but        := (OTHERS => '0');
        v_uns_half_index    := (OTHERS => '0');
        
    ELSIF RISING_EDGE(P_clk_i) THEN
    
        CASE sm_fft_status IS
        
            WHEN status_fft_idle =>
        
                -- Start FFT
                IF (P_start_i = '1') THEN 
                
                    -- JUMP to next state
                    sm_fft_status <= status_fft_read_samples_bit_reverse_idx;

                    -- FFT is now in a busy state
                    P_busy_o <= '1'; 
                    
                    -- Reset all signals before performing FFT                    
                    s_sig_write_fft_ram_real        <= (OTHERS => '0');
                    s_sig_write_fft_ram_imag        <= (OTHERS => '0');
                    s_uns_read_adr_ram_idx             <= (OTHERS => '0');
                    s_uns_write_adr_ram_idx            <= (OTHERS => '1');
                    s_uns_read_adr_sin_cos_rom_idx    <= (OTHERS => '1');
                    s_uns_read_spectrum_idx            <= (OTHERS => '1');
                    s_std_we                        <= '0';
                    s_std_re                        <= '0';
                    
                    -- Update variables
                    v_uns_stage     := TO_UNSIGNED(1, v_uns_stage'LENGTH);
                    v_int_atten     := TO_INTEGER(UNSIGNED(P_atten_i));
                    v_uns_temp_adr    := (OTHERS => '0');
                    v_uns_cpt_adr    := (OTHERS => '1');
                    
                    -- Clear done signal
                    P_done_o <= '0';
                
                ELSE    
                
                    -- Wait ...
                    
                    -- Reset all signals before performing FFT                    
                    s_sig_write_fft_ram_real        <= (OTHERS => '0');
                    s_sig_write_fft_ram_imag        <= (OTHERS => '0');
                    s_uns_read_adr_ram_idx             <= (OTHERS => '0');
                    s_uns_write_adr_ram_idx            <= (OTHERS => '1');
                    s_uns_read_adr_sin_cos_rom_idx    <= (OTHERS => '1');
                    s_uns_read_spectrum_idx            <= (OTHERS => '1');
                    s_std_we                        <= '0';
                    s_std_re                        <= '0';
                    
                    -- Update variables
                    v_uns_stage     := TO_UNSIGNED(1, v_uns_stage'LENGTH);
                    v_int_atten     := TO_INTEGER(UNSIGNED(P_atten_i));
                    v_uns_temp_adr    := (OTHERS => '0');
                    v_uns_cpt_adr    := (OTHERS => '1');
                    
                    -- Clear done signal
                    P_done_o <= '0';

                END IF;
            
            WHEN status_fft_read_samples_bit_reverse_idx  =>
            
                IF ( (v_uns_cpt_adr = TO_UNSIGNED(C_MEM_IDX_MAX, v_uns_cpt_adr'LENGTH)) AND (s_std_we = '1') ) THEN
                --IF ( (s_uns_write_adr_ram_idx = TO_UNSIGNED(C_MEM_IDX_MAX, s_uns_write_adr_ram_idx'LENGTH)) AND (s_std_we = '1') ) THEN
                
                    -- JUMP to next state
                    sm_fft_status <= status_fft_update_stage;
                    
                    -- Read last values
                    --s_sig_write_fft_ram_real <= SIGNED(P_data_re_i);
                    --s_sig_write_fft_ram_imag <= SIGNED(P_data_im_i);
                    
                    -- W disable
                    s_std_we <= '0'; 
                    
                ELSE
                
                    -- Write enable
                    s_std_we <= '1'; 
                    
                    -- Read input samples and save them into RAM
                    s_sig_write_fft_ram_real <= SIGNED(P_data_re_i);
                    s_sig_write_fft_ram_imag <= SIGNED(P_data_im_i);
                    
                    -- Rearrange W address indexes for bit reversing        
                    --v_uns_cpt_adr := v_uns_cpt_adr + 1;    -- Initialized to all '1' so roll-over to 0
                    
                    --FOR i IN v_uns_temp_adr'RANGE LOOP
                        --v_uns_temp_adr(i) := v_uns_cpt_adr(v_uns_cpt_adr'LENGTH - 1 - i);
                    --END LOOP;
                    
                    --s_uns_write_adr_ram_idx <= v_uns_temp_adr;
                    
                    v_uns_cpt_adr := v_uns_cpt_adr + 1;    -- Initialized to all '1' so roll-over to 0
                    s_uns_write_adr_ram_idx <= FCT_bit_reverse_value(v_uns_cpt_adr);
                
                END IF;
            
            
            WHEN status_fft_update_stage  =>
            
                -- Update loop variables
                v_uns_stage     := SHIFT_LEFT(v_uns_stage, 1);         --1st step, 2, 4, 8, 16, 32, 64, 128, ...
                v_uns_points     := v_uns_stage;                     --2nd step, 2, 4, 8, 16, 32, 64, 128, ...
                v_uns_butterfly    := SHIFT_RIGHT(v_uns_points, 1);     --3rd step, 1, 2, 4, 8,  16, 32, 64, ...
                
                -- Check max stages
                IF (v_uns_stage <= TO_UNSIGNED(C_NB_POINTS_MAX, v_uns_stage'LENGTH)) THEN
                                    
                    -- JUMP to next state
                    sm_fft_status <= status_fft_loop_butterfly_per_group;
                    
                    -- Initialize index for next state
                    v_uns_loop_grp := TO_UNSIGNED(0, v_uns_loop_grp'LENGTH);
                
                ELSE    
                    
                    -- FFT finished
                    
                    -- JUMP to next state
                    sm_fft_status <= status_fft_pipeline;
                    
                    -- Prepare output samples
                    s_std_re                 <= '1'; 
                    s_uns_read_adr_ram_idx     <= TO_UNSIGNED(0, s_uns_read_adr_ram_idx'LENGTH);
    
                END IF;
                
                
            WHEN status_fft_loop_butterfly_per_group  =>
            
                -- Check max loop
                IF (v_uns_loop_grp < v_uns_butterfly) THEN
                
                    -- JUMP to next state
                    sm_fft_status <= status_fft_perform_butterfly;
                    
                    -- Initialize butterfly state
                    sm_butterfly_step <= butterfly_pipeline_1;
                    
                    -- Update loop indexes
                    v_uns_index_but     := v_uns_loop_grp; -- Initialize index for next state
                    v_uns_half_index     := v_uns_index_but + v_uns_butterfly; -- Compute half index for RAM read
                    
                    -- Prepare reading RAM/ROM for next state
                    s_std_re                 <= '1'; -- R enable
                    s_uns_read_adr_ram_idx     <= RESIZE(v_uns_half_index, s_uns_read_adr_ram_idx'LENGTH); -- Read RAM
                    
                    --IF( (s_uns_read_adr_sin_cos_rom_idx = TO_UNSIGNED(0, s_uns_read_adr_sin_cos_rom_idx'LENGTH))  AND (s_bool_read_offset_0 = FALSE) ) THEN
                        --s_uns_read_adr_sin_cos_rom_idx     <= TO_UNSIGNED(0, s_uns_read_adr_sin_cos_rom_idx'LENGTH);
                        --s_bool_read_offset_0            <= TRUE;
                    --ELSE
                        --s_uns_read_adr_sin_cos_rom_idx <= s_uns_read_adr_sin_cos_rom_idx + 1;
                    --END IF;
                    
                    -- SIN/COS ROM : starts from 0 and it's incremented by 1 inside this loop only
                    s_uns_read_adr_sin_cos_rom_idx <= s_uns_read_adr_sin_cos_rom_idx + 1; -- Initialized to all '1' so roll-over to 0
                                        
                    -- Increment loop index
                    v_uns_loop_grp := v_uns_loop_grp + 1;
    
                ELSE
                
                    -- JUMP to next state
                    sm_fft_status <= status_fft_update_stage;

                
                END IF;
                
                
            WHEN status_fft_perform_butterfly  =>
            
                -- Check max butterfly
                IF (v_uns_index_but < TO_UNSIGNED(C_NB_POINTS_MAX, v_uns_index_but'LENGTH)) THEN
                
-- Butterfly complex operations for radix-2 decimation in time (DIT):

-- T                    = W(s_uns_read_adr_sin_cos_rom_idx) * X(v_uns_half_index)
-- X(v_uns_half_index)     = X(v_uns_index_but) - T 
-- X(v_uns_index_but)     = X(v_uns_index_but) + T

-- Split into real and imaginary parts :

-- v_temp_re = s_stdlv_cos(s_uns_read_adr_sin_cos_rom_idx) * s_stdlv_read_fft_ram_real(v_uns_half_index) - s_stdlv_sin(s_uns_read_adr_sin_cos_rom_idx) * s_stdlv_read_fft_ram_imag(v_uns_half_index)
--           = v_temp_a - v_temp_b (or "+" if IFFT)

-- v_temp_im = s_stdlv_cos(s_uns_read_adr_sin_cos_rom_idx) * s_stdlv_read_fft_ram_imag(v_uns_half_index) + s_stdlv_sin(s_uns_read_adr_sin_cos_rom_idx) * s_stdlv_read_fft_ram_real(v_uns_half_index)
--           = v_temp_c + v_temp_d (or "-" if IFFT)

-- s_sig_write_fft_ram_real(v_uns_half_index) = s_stdlv_read_fft_ram_real(v_uns_index_but) - v_temp_re
-- s_sig_write_fft_ram_imag(v_uns_half_index) = s_stdlv_read_fft_ram_imag(v_uns_index_but) - v_temp_im

-- s_sig_write_fft_ram_real(v_uns_index_but) = s_stdlv_read_fft_ram_real(v_uns_index_but) + v_temp_re
-- s_sig_write_fft_ram_imag(v_uns_index_but) = s_stdlv_read_fft_ram_imag(v_uns_index_but) + v_temp_im
                    
                    CASE sm_butterfly_step IS
                    
                        WHEN butterfly_pipeline_1  =>
                        
                            -- UNCHANGED signals
                            -- Wait one cycle for RAM/ROM to be read
                            
                            s_std_we <= '0'; -- W disable
                            
                            -- JUMP to next state
                            sm_butterfly_step <= butterfly_step_cplx_mult_a;
                    
                        WHEN butterfly_step_cplx_mult_a  =>
                        
                            v_temp_a := RESIZE( SIGNED(s_stdlv_cos(s_stdlv_cos'LEFT DOWNTO (G_BITS_IN - 1 - G_SHIFT))), G_BITS_IN ) * SIGNED(s_stdlv_read_fft_ram_real);
                            
                            -- JUMP to next state
                            sm_butterfly_step <= butterfly_step_cplx_mult_b;
                        
                        WHEN butterfly_step_cplx_mult_b  =>
                        
                            v_temp_b := RESIZE( SIGNED(s_stdlv_sin(s_stdlv_sin'LEFT DOWNTO (G_BITS_IN - 1 - G_SHIFT))), G_BITS_IN ) * SIGNED(s_stdlv_read_fft_ram_imag);
                            
                            -- JUMP to next state
                            sm_butterfly_step <= butterfly_step_cplx_mult_c;
                        
                        WHEN butterfly_step_cplx_mult_c  =>
                        
                            v_temp_c := RESIZE( SIGNED(s_stdlv_cos(s_stdlv_cos'LEFT DOWNTO (G_BITS_IN - 1 - G_SHIFT))), G_BITS_IN ) * SIGNED(s_stdlv_read_fft_ram_imag);
                            
                            -- JUMP to next state
                            sm_butterfly_step <= butterfly_step_cplx_mult_d;
                        
                        WHEN butterfly_step_cplx_mult_d  =>
                        
                            v_temp_d := RESIZE( SIGNED(s_stdlv_sin(s_stdlv_sin'LEFT DOWNTO (G_BITS_IN - 1 - G_SHIFT))), G_BITS_IN ) * SIGNED(s_stdlv_read_fft_ram_real);
                            
                            -- JUMP to next state
                            sm_butterfly_step <= butterfly_step_cplx_mult_res;
                            
                        WHEN butterfly_step_cplx_mult_res  =>
                        
                            IF (P_ifft_i = '0') THEN
                            
                                --FFT
                                v_temp_re := v_temp_a - v_temp_b; 
                                v_temp_im := v_temp_c + v_temp_d;
                            ELSE
                                
                                --IFFT
                                v_temp_re := v_temp_a + v_temp_b;
                                v_temp_im := v_temp_c - v_temp_d; 
                            END IF;
                            
                            -- Shift result
                            v_temp_re := RESIZE(v_temp_re(v_temp_re'LEFT DOWNTO G_SHIFT), v_temp_re'LENGTH);
                            v_temp_im := RESIZE(v_temp_im(v_temp_im'LEFT DOWNTO G_SHIFT), v_temp_im'LENGTH);
                            
                            -- Test overflow
                            IF ( G_CLIP = TRUE ) THEN
                                
                                IF (v_temp_re > C_CLIP_MAX) THEN
                                    v_temp_re := C_CLIP_MAX;
                                END IF;
                                    
                                IF (v_temp_im > C_CLIP_MAX) THEN
                                    v_temp_im := C_CLIP_MAX;
                                END IF;
                                
                                IF (v_temp_re < C_CLIP_MIN) THEN
                                    v_temp_re := C_CLIP_MIN;
                                END IF;
                                
                                IF (v_temp_im < C_CLIP_MIN) THEN
                                    v_temp_im := C_CLIP_MIN;
                                END IF;
                                    
                            END IF; 

                            -- Update RAM addresses
                            s_uns_read_adr_ram_idx <= RESIZE(v_uns_index_but, s_uns_read_adr_ram_idx'LENGTH); -- Read RAM
                            
                            -- JUMP to next state
                            sm_butterfly_step <= butterfly_pipeline_2;
                        
                        WHEN butterfly_pipeline_2  =>
                        
                            -- UNCHANGED signals
                            -- Wait one cycle for RAM to be read
                            
                            -- JUMP to next state
                            sm_butterfly_step <= butterfly_step_cplx_sum_half_read;
                            
                        WHEN butterfly_step_cplx_sum_half_read  =>
                                            
                            v_shift_re         := RESIZE(SIGNED(s_stdlv_read_fft_ram_real), v_shift_re'LENGTH) - v_temp_re;
                            v_shift_re         := RESIZE(v_shift_re(v_shift_re'LEFT DOWNTO v_int_atten), v_shift_re'LENGTH); -- Attenuation
                            v_scale_re_ifft := RESIZE(v_shift_re(v_shift_re'LEFT DOWNTO 1), v_shift_re'LENGTH); -- Divide by 2 for an IFFT
                            
                            -- Test overflow
                            IF ( G_CLIP = TRUE ) THEN
                                
                                IF (v_shift_re > C_CLIP_MAX) THEN
                                    v_shift_re := C_CLIP_MAX;
                                END IF;
                                    
                                IF (v_scale_re_ifft > C_CLIP_MAX) THEN
                                    v_scale_re_ifft := C_CLIP_MAX;
                                END IF;
                                
                                IF (v_shift_re < C_CLIP_MIN) THEN
                                    v_shift_re := C_CLIP_MIN;
                                END IF;
                                
                                IF (v_scale_re_ifft < C_CLIP_MIN) THEN
                                    v_scale_re_ifft := C_CLIP_MIN;
                                END IF;
                                    
                            END IF;                         
                            
                            v_shift_im         := RESIZE(SIGNED(s_stdlv_read_fft_ram_imag), v_shift_im'LENGTH) - v_temp_im;
                            v_shift_im         := RESIZE(v_shift_im(v_shift_im'LEFT DOWNTO v_int_atten), v_shift_im'LENGTH); -- Attenuation
                            v_scale_im_ifft := RESIZE(v_shift_im(v_shift_im'LEFT DOWNTO 1), v_shift_im'LENGTH); -- Divide by 2 for an IFFT
                            
                            -- Test overflow
                            IF ( G_CLIP = TRUE ) THEN
                                
                                IF (v_shift_im > C_CLIP_MAX) THEN
                                    v_shift_im := C_CLIP_MAX;
                                END IF;
                                    
                                IF (v_scale_im_ifft > C_CLIP_MAX) THEN
                                    v_scale_im_ifft := C_CLIP_MAX;
                                END IF;
                                
                                IF (v_shift_im < C_CLIP_MIN) THEN
                                    v_shift_im := C_CLIP_MIN;
                                END IF;
                                
                                IF (v_scale_im_ifft < C_CLIP_MIN) THEN
                                    v_scale_im_ifft := C_CLIP_MIN;
                                END IF;
                                    
                            END IF;                                     
                            
                            -- JUMP to next state
                            sm_butterfly_step <= butterfly_step_cplx_sum_half_write;
                            
                        
                        WHEN butterfly_step_cplx_sum_half_write  =>
                        
                            s_std_we <= '1'; -- W enable
                            s_uns_write_adr_ram_idx <= RESIZE(v_uns_half_index, s_uns_write_adr_ram_idx'LENGTH); -- Write RAM
                            
                            IF (P_ifft_i = '0') THEN
                                
                                --FFT
                                s_sig_write_fft_ram_real <= RESIZE(v_shift_re, s_sig_write_fft_ram_real'LENGTH); 
                                s_sig_write_fft_ram_imag <= RESIZE(v_shift_im, s_sig_write_fft_ram_imag'LENGTH);                                
                            ELSE
                            
                                --IFFT
                                s_sig_write_fft_ram_real <= RESIZE(v_scale_re_ifft, s_sig_write_fft_ram_real'LENGTH); 
                                s_sig_write_fft_ram_imag <= RESIZE(v_scale_im_ifft, s_sig_write_fft_ram_imag'LENGTH); 
                            END IF;
                        
                            -- JUMP to next state
                            sm_butterfly_step <= butterfly_pipeline_3;
                            
                            -- Update RAM addresses
                            s_uns_read_adr_ram_idx     <= RESIZE(v_uns_index_but, s_uns_read_adr_ram_idx'LENGTH); -- Read RAM
                            
                            
                        WHEN butterfly_pipeline_3  =>
                        
                            -- UNCHANGED signals
                            -- Wait one cycle for RAM to be read
                            
                            s_std_we <= '0'; -- W disable
                            
                            -- JUMP to next state
                            sm_butterfly_step <= butterfly_step_cplx_sum_curr_read;
                        
                        WHEN butterfly_step_cplx_sum_curr_read  =>
                        
                            v_shift_re         := RESIZE(SIGNED(s_stdlv_read_fft_ram_real), v_shift_re'LENGTH) + v_temp_re;
                            v_shift_re         := RESIZE(v_shift_re(v_shift_re'LEFT DOWNTO v_int_atten), v_shift_re'LENGTH); -- Attenuation
                            v_scale_re_ifft := RESIZE(v_shift_re(v_shift_re'LEFT DOWNTO 1), v_shift_re'LENGTH); -- Divide by 2 for an IFFT
                            
                            -- Test overflow
                            IF ( G_CLIP = TRUE ) THEN
                                
                                IF (v_shift_re > C_CLIP_MAX) THEN
                                    v_shift_re := C_CLIP_MAX;
                                END IF;
                                    
                                IF (v_scale_re_ifft > C_CLIP_MAX) THEN
                                    v_scale_re_ifft := C_CLIP_MAX;
                                END IF;
                                
                                IF (v_shift_re < C_CLIP_MIN) THEN
                                    v_shift_re := C_CLIP_MIN;
                                END IF;
                                
                                IF (v_scale_re_ifft < C_CLIP_MIN) THEN
                                    v_scale_re_ifft := C_CLIP_MIN;
                                END IF;
                                    
                            END IF;     
                                                    
                            v_shift_im         := RESIZE(SIGNED(s_stdlv_read_fft_ram_imag), v_shift_im'LENGTH) + v_temp_im;
                            v_shift_im         := RESIZE(v_shift_im(v_shift_im'LEFT DOWNTO v_int_atten), v_shift_im'LENGTH); -- Attenuation
                            v_scale_im_ifft := RESIZE(v_shift_im(v_shift_im'LEFT DOWNTO 1), v_shift_im'LENGTH); -- Divide by 2 for an IFFT
                            
                            -- Test overflow
                            IF ( G_CLIP = TRUE ) THEN
                                
                                IF (v_shift_im > C_CLIP_MAX) THEN
                                    v_shift_im := C_CLIP_MAX;
                                END IF;
                                    
                                IF (v_scale_im_ifft > C_CLIP_MAX) THEN
                                    v_scale_im_ifft := C_CLIP_MAX;
                                END IF;
                                
                                IF (v_shift_im < C_CLIP_MIN) THEN
                                    v_shift_im := C_CLIP_MIN;
                                END IF;
                                
                                IF (v_scale_im_ifft < C_CLIP_MIN) THEN
                                    v_scale_im_ifft := C_CLIP_MIN;
                                END IF;
                                    
                            END IF; 
                            
                            -- JUMP to next state
                            sm_butterfly_step <= butterfly_step_cplx_sum_curr_write;
                            
                        WHEN butterfly_step_cplx_sum_curr_write  =>
                        
                            -- W enable
                            s_std_we <= '1';
                            s_uns_write_adr_ram_idx <= RESIZE(v_uns_index_but, s_uns_write_adr_ram_idx'LENGTH); -- Write RAM
                            
                            IF (P_ifft_i = '0') THEN
                            
                                --FFT
                                s_sig_write_fft_ram_real <= RESIZE(v_shift_re, s_sig_write_fft_ram_real'LENGTH);  
                                s_sig_write_fft_ram_imag <= RESIZE(v_shift_im, s_sig_write_fft_ram_imag'LENGTH);
                            ELSE
                            
                                --IFFT
                                s_sig_write_fft_ram_real <= RESIZE(v_scale_re_ifft, s_sig_write_fft_ram_real'LENGTH); 
                                s_sig_write_fft_ram_imag <= RESIZE(v_scale_im_ifft, s_sig_write_fft_ram_imag'LENGTH);
                            END IF;
                            
                            -- Update loop indexes
                            v_uns_index_but     := v_uns_index_but + v_uns_points; 
                            v_uns_half_index := v_uns_index_but + v_uns_butterfly; -- Compute half index
                            
                            -- Prepare for next state
                            s_std_re                 <= '1'; -- R enable
                            s_uns_read_adr_ram_idx     <= RESIZE(v_uns_half_index, s_uns_read_adr_ram_idx'LENGTH); -- Read RAM
                            
                            -- Initialize butterfly state
                            sm_butterfly_step <= butterfly_pipeline_1;
                                                    
                    END CASE;
                                        
                    
                ELSE
                
                    -- JUMP to next state
                    sm_fft_status <= status_fft_loop_butterfly_per_group;
    
                END IF;

            
            WHEN status_fft_pipeline  =>
            
                -- UNCHANGED signals
                -- Wait one cycle for RAM to be read
            
                -- JUMP to next state
                sm_fft_status <= status_fft_done;
                
                s_uns_read_adr_ram_idx <= s_uns_read_adr_ram_idx + 1;
                
                    
            WHEN status_fft_done  =>    

                IF ( (s_uns_read_spectrum_idx = TO_UNSIGNED(C_MEM_IDX_MAX, s_uns_read_spectrum_idx'LENGTH)) AND (s_std_re = '0') ) THEN
                
                    -- JUMP to next state
                    sm_fft_status <= status_fft_idle;
                    
                    -- Data out done
                    P_done_o <= '0';
                    
                    -- Release from busy state
                    P_busy_o <= '0';
                    
                ELSE
                
                    -- Data out valid
                    P_done_o <= '1';
                    
                    -- Read RAM samples
                    P_data_re_o    <= STD_LOGIC_VECTOR(RESIZE(SIGNED(s_stdlv_read_fft_ram_real), P_data_re_o'LENGTH));
                    P_data_im_o <= STD_LOGIC_VECTOR(RESIZE(SIGNED(s_stdlv_read_fft_ram_imag), P_data_im_o'LENGTH));
                
                    -- Wait for RAM to be read
                    s_uns_read_spectrum_idx <= s_uns_read_spectrum_idx + 1; -- Initialized to all '1' so roll-over to 0
                
                END IF;
                
                -- Read RAM
                IF (s_uns_read_adr_ram_idx = TO_UNSIGNED(C_MEM_IDX_MAX, s_uns_read_adr_ram_idx'LENGTH)) THEN
                                            
                    -- R disable
                    s_std_re <= '0';
                    
                ELSE
                                    
                    s_uns_read_adr_ram_idx     <= s_uns_read_adr_ram_idx + 1;
                
                END IF;
            
            
        END CASE;
    
    
    END IF;
        
    END PROCESS proc_main;
    
                    
END rtl;
