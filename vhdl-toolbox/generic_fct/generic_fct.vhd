-- BRIEF    : Useful functions for the project
-- DATE     : March 23th, 2015
-- AUTHOR   : O.D

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.MATH_REAL.ALL;

LIBRARY WORK;
USE WORK.proj_config.ALL;

PACKAGE generic_fct IS

    FUNCTION FCT_check_ovf_pos (input : STD_LOGIC_VECTOR; len_min : POSITIVE) RETURN BOOLEAN;
    FUNCTION FCT_check_ovf_neg (input : STD_LOGIC_VECTOR; len_min : POSITIVE) RETURN BOOLEAN;
    FUNCTION FCT_clip (input : STD_LOGIC_VECTOR; len_min : POSITIVE; clip_min : STD_LOGIC_VECTOR; clip_max : STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR;
    FUNCTION FCT_bit_reverse_value (input : UNSIGNED) RETURN UNSIGNED;
    FUNCTION FCT_init_cos_table RETURN t_sin_cos_table;
    FUNCTION FCT_init_sin_table RETURN t_sin_cos_table;
    FUNCTION FCT_init_crc32_d8_lut_msb RETURN t_crc_table;
    FUNCTION FCT_init_crc32_d8_lut_lsb RETURN t_crc_table;
    FUNCTION FCT_get_crc32_d4 (data: STD_LOGIC_VECTOR(3 DOWNTO 0); crc:  STD_LOGIC_VECTOR(31 DOWNTO 0)) RETURN STD_LOGIC_VECTOR;

END PACKAGE generic_fct;


PACKAGE BODY generic_fct IS

    FUNCTION FCT_check_ovf_pos (input : STD_LOGIC_VECTOR; len_min : POSITIVE) RETURN BOOLEAN IS

        VARIABLE v_std_ret             : STD_LOGIC := '0';
        CONSTANT c_std_neg_sign     : STD_LOGIC := '1';
        CONSTANT c_std_no_pos_ovf     : STD_LOGIC := '0';
    
    BEGIN
    
        IF (input(input'LEFT) = c_std_neg_sign) THEN     
            RETURN FALSE;    --Return FALSE if the value is negative
        END IF;
        
        FOR i IN input'LEFT DOWNTO len_min LOOP
            v_std_ret := input(i) OR v_std_ret;
        END LOOP;
        
        IF (v_std_ret = c_std_no_pos_ovf) THEN
            RETURN FALSE;
        ELSE
            RETURN TRUE;
        END IF;
        
    END FUNCTION FCT_check_ovf_pos;
    
    FUNCTION FCT_check_ovf_neg (input : STD_LOGIC_VECTOR; len_min : POSITIVE) RETURN BOOLEAN IS

        VARIABLE v_std_ret             : STD_LOGIC := '1';
        CONSTANT c_std_pos_sign     : STD_LOGIC := '0';
        CONSTANT c_std_no_neg_ovf     : STD_LOGIC := '1';
    
    BEGIN
    
        IF (input(input'LEFT) = c_std_pos_sign) THEN     
            RETURN FALSE;    --Return FALSE if the value is positive
        END IF;
        
        FOR i IN input'LEFT DOWNTO len_min LOOP
            v_std_ret := input(i) AND v_std_ret;
        END LOOP;
        
        IF (v_std_ret = c_std_no_neg_ovf) THEN
            RETURN FALSE;
        ELSE
            RETURN TRUE;
        END IF;
        
    END FUNCTION FCT_check_ovf_neg;
    
    FUNCTION FCT_clip (input : STD_LOGIC_VECTOR; len_min : POSITIVE; clip_min : STD_LOGIC_VECTOR; clip_max : STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS
        
        VARIABLE v_stdlv_val : STD_LOGIC_VECTOR(input'LEFT DOWNTO 0) := input;

    BEGIN    
    
        IF (FCT_check_ovf_pos(v_stdlv_val,len_min)) THEN
            RETURN clip_max;
        END IF;
        
        IF (FCT_check_ovf_neg(v_stdlv_val,len_min)) THEN
            RETURN clip_min;
        END IF;

        RETURN v_stdlv_val;
        
    END FUNCTION FCT_clip;
    
    FUNCTION FCT_bit_reverse_value (input : UNSIGNED) RETURN UNSIGNED IS

        VARIABLE v_uns_temp_adr    : UNSIGNED(input'LEFT DOWNTO 0);
    
    BEGIN
        
        FOR i IN v_uns_temp_adr'RANGE LOOP
            v_uns_temp_adr(i) := input(input'LEFT - i);
        END LOOP;
        
        RETURN v_uns_temp_adr;
        
    END FUNCTION FCT_bit_reverse_value;
    
    
    FUNCTION FCT_init_cos_table RETURN t_sin_cos_table IS
    
        VARIABLE v_cos     : t_sin_cos_table;
        VARIABLE L         : POSITIVE := 2;
        VARIABLE k         : INTEGER := 0;
        VARIABLE theta     : REAL;
        
    BEGIN
    
        WHILE (L <= 2**C_MEM_POWER_2_DEPTH) LOOP
        
            theta := (2.0*MATH_PI)/REAL(L);
            
            FOR i IN 0 TO ((L/2) - 1) LOOP
            
                v_cos(k) := STD_LOGIC_VECTOR    (
                            TO_SIGNED        ( 
                            INTEGER        (
                            ROUND    ( 
                                    REAL( (2**(C_MEM_DATA_LENGTH - 1)) - 3 ) * COS( REAL(i)*theta )
                                    )
                                        ), C_MEM_DATA_LENGTH
                                            )
                                                );
                                            
                k := k + 1;
                
            END LOOP;
            
            L := L*2;
            
        END LOOP;
        
        v_cos((2**C_MEM_POWER_2_DEPTH) - 1) := STD_LOGIC_VECTOR( TO_SIGNED(0, C_MEM_DATA_LENGTH) );
        
        RETURN v_cos;
    
    END FUNCTION FCT_init_cos_table;
    
    
    FUNCTION FCT_init_sin_table RETURN t_sin_cos_table IS
    
        VARIABLE v_sin     : t_sin_cos_table;
        VARIABLE L         : POSITIVE := 2;
        VARIABLE k         : INTEGER := 0;
        VARIABLE theta     : REAL;
        
    BEGIN
    
        WHILE (L <= 2**C_MEM_POWER_2_DEPTH) LOOP
        
            theta := (2.0*MATH_PI)/REAL(L);
            
            FOR i IN 0 TO ((L/2) - 1) LOOP
            
                v_sin(k) := STD_LOGIC_VECTOR    (
                            TO_SIGNED        ( 
                            INTEGER        (
                            ROUND    ( 
                                    - REAL( (2**(C_MEM_DATA_LENGTH - 1)) - 3 ) * SIN( REAL(i)*theta )
                                    )
                                        ), C_MEM_DATA_LENGTH
                                            )
                                                );
                k := k + 1;
            
            END LOOP;
            
            L := L*2;
            
        END LOOP;
        
        v_sin((2**C_MEM_POWER_2_DEPTH) - 1) := STD_LOGIC_VECTOR( TO_SIGNED(0, C_MEM_DATA_LENGTH) );
        
        RETURN v_sin;
    
    END FUNCTION FCT_init_sin_table;
    
    
    FUNCTION FCT_init_crc32_d8_lut_msb RETURN t_crc_table IS
    
        VARIABLE v_msb                     : t_crc_table;
        VARIABLE v_stdlv_crc32_accum     : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
        
    BEGIN
        
        FOR i IN 0 TO 255 LOOP
            
            v_stdlv_crc32_accum    := STD_LOGIC_VECTOR( SHIFT_LEFT( TO_UNSIGNED(i, v_stdlv_crc32_accum'LENGTH), 24 ) );
            
            FOR j IN 0 TO 7 LOOP
            
                IF (v_stdlv_crc32_accum(31) = '1') THEN
                    v_stdlv_crc32_accum    := STD_LOGIC_VECTOR(SHIFT_LEFT(UNSIGNED(v_stdlv_crc32_accum), 1)) XOR C_CRC_POLYNOMIAL32;
                ELSE
                    v_stdlv_crc32_accum    := STD_LOGIC_VECTOR(SHIFT_LEFT(UNSIGNED(v_stdlv_crc32_accum), 1));
                END IF;
            
            END LOOP;
            
            v_msb(i) := v_stdlv_crc32_accum(31 DOWNTO 16);
        
        END LOOP;
    
        RETURN v_msb;
    
    END FUNCTION FCT_init_crc32_d8_lut_msb;
    
    
    FUNCTION FCT_init_crc32_d8_lut_lsb RETURN t_crc_table IS
    
        VARIABLE v_lsb                     : t_crc_table;
        VARIABLE v_stdlv_crc32_accum     : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
        
    BEGIN
        
        FOR i IN 0 TO 255 LOOP
            
            v_stdlv_crc32_accum    := STD_LOGIC_VECTOR( SHIFT_LEFT( TO_UNSIGNED(i, v_stdlv_crc32_accum'LENGTH), 24 ) );
            
            FOR j IN 0 TO 7 LOOP
            
                IF (v_stdlv_crc32_accum(31) = '1') THEN
                    v_stdlv_crc32_accum    := STD_LOGIC_VECTOR(SHIFT_LEFT(UNSIGNED(v_stdlv_crc32_accum), 1)) XOR C_CRC_POLYNOMIAL32;
                ELSE
                    v_stdlv_crc32_accum    := STD_LOGIC_VECTOR(SHIFT_LEFT(UNSIGNED(v_stdlv_crc32_accum), 1));
                END IF;
            
            END LOOP;
            
            v_lsb(i) := v_stdlv_crc32_accum(15 DOWNTO 0);
        
        END LOOP;
    
        RETURN v_lsb;
    
    END FUNCTION FCT_init_crc32_d8_lut_lsb;
    
    -- http://www.easics.com/webtools/crctool
    -- polynomial: (0 1 2 4 5 7 8 10 11 12 16 22 23 26 32)
    -- data width: 4
    -- convention: the first serial bit is D[3]
    FUNCTION FCT_get_crc32_d4 (data: STD_LOGIC_VECTOR(3 DOWNTO 0); crc:  STD_LOGIC_VECTOR(31 DOWNTO 0)) RETURN STD_LOGIC_VECTOR IS

        VARIABLE d:      STD_LOGIC_VECTOR(3 DOWNTO 0);
        VARIABLE c:      STD_LOGIC_VECTOR(31 DOWNTO 0);
        VARIABLE newcrc: STD_LOGIC_VECTOR(31 DOWNTO 0);

    BEGIN
    
        d := data;
        c := crc;

        newcrc(0) := d(0) xor c(28);
        newcrc(1) := d(1) xor d(0) xor c(28) xor c(29);
        newcrc(2) := d(2) xor d(1) xor d(0) xor c(28) xor c(29) xor c(30);
        newcrc(3) := d(3) xor d(2) xor d(1) xor c(29) xor c(30) xor c(31);
        newcrc(4) := d(3) xor d(2) xor d(0) xor c(0) xor c(28) xor c(30) xor c(31);
        newcrc(5) := d(3) xor d(1) xor d(0) xor c(1) xor c(28) xor c(29) xor c(31);
        newcrc(6) := d(2) xor d(1) xor c(2) xor c(29) xor c(30);
        newcrc(7) := d(3) xor d(2) xor d(0) xor c(3) xor c(28) xor c(30) xor c(31);
        newcrc(8) := d(3) xor d(1) xor d(0) xor c(4) xor c(28) xor c(29) xor c(31);
        newcrc(9) := d(2) xor d(1) xor c(5) xor c(29) xor c(30);
        newcrc(10) := d(3) xor d(2) xor d(0) xor c(6) xor c(28) xor c(30) xor c(31);
        newcrc(11) := d(3) xor d(1) xor d(0) xor c(7) xor c(28) xor c(29) xor c(31);
        newcrc(12) := d(2) xor d(1) xor d(0) xor c(8) xor c(28) xor c(29) xor c(30);
        newcrc(13) := d(3) xor d(2) xor d(1) xor c(9) xor c(29) xor c(30) xor c(31);
        newcrc(14) := d(3) xor d(2) xor c(10) xor c(30) xor c(31);
        newcrc(15) := d(3) xor c(11) xor c(31);
        newcrc(16) := d(0) xor c(12) xor c(28);
        newcrc(17) := d(1) xor c(13) xor c(29);
        newcrc(18) := d(2) xor c(14) xor c(30);
        newcrc(19) := d(3) xor c(15) xor c(31);
        newcrc(20) := c(16);
        newcrc(21) := c(17);
        newcrc(22) := d(0) xor c(18) xor c(28);
        newcrc(23) := d(1) xor d(0) xor c(19) xor c(28) xor c(29);
        newcrc(24) := d(2) xor d(1) xor c(20) xor c(29) xor c(30);
        newcrc(25) := d(3) xor d(2) xor c(21) xor c(30) xor c(31);
        newcrc(26) := d(3) xor d(0) xor c(22) xor c(28) xor c(31);
        newcrc(27) := d(1) xor c(23) xor c(29);
        newcrc(28) := d(2) xor c(24) xor c(30);
        newcrc(29) := d(3) xor c(25) xor c(31);
        newcrc(30) := c(26);
        newcrc(31) := c(27);
        
        RETURN newcrc;
        
    END FUNCTION FCT_get_crc32_d4;
    
END PACKAGE BODY generic_fct;
