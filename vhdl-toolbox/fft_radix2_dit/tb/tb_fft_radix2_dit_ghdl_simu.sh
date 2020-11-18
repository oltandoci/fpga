rm *.ghw
rm *.cf
ghdl -a ../../proj_config/proj_config.vhd
ghdl -a ../../generic_fct/generic_fct.vhd
ghdl -a ../../ram/src/ram_cmp.vhd
ghdl -a ../../ram/src/ram.vhd
ghdl -a ../../rom/src/sin_cos_rom_cmp.vhd
ghdl -a ../../rom/src/sin_cos_rom.vhd
ghdl -a ../../fft_radix2_dit/src/fft_radix2_dit_cmp.vhd
ghdl -a ../../fft_radix2_dit/src/fft_radix2_dit.v3synth.vhd
ghdl -a --ieee=synopsys tb_fft_radix2_dit.vhd
ghdl -e --ieee=synopsys tb_fft_radix2_dit
ghdl -r --ieee=synopsys tb_fft_radix2_dit --wave=run.ghw
gtkwave run.ghw
