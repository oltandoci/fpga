1. Get python toolbox:
   git clone https://github.com/oltandoci/python.git

2. Copy python/pytoolbox/ here if you don't want to modify relative paths,
   or put it in your personal folder which is in the PATH (a good solution).
   In this case just update/remove the import... paths
   
3. Generate input stimuli for VHDL tb: 
   ./fft_radix2_dit_test_vector.py
   
4. Run VHDL tb: 
   cd ../../fft_radix2_dit/tb
   ./tb_fft_radix2_dit_ghdl_simu.sh
   
5. Check some signals on GTK Wave:

* Get signals
- From the SST pane, expand the "top" entity
- Clik on "p_data_re_o", it will be put in the Signals pane below
- Just drag and drop it in the Sigals pane above under the Time
- Or directly, a right clik on the signal and then:
  Recurse Import -> Append (or Insert) -> Yes
- Do the same with the signal "p_data_im_o"

* Format the signals
- On the Signals pane under the Time, right clik on each signal and then:
  Data Format -> Signed Decimal
  Data Format -> Analog -> Step
  
* Zoom utilities
- Insert some margins between each signal:
  Menu: Edit -> Insert Analog Height Extension
  Or directly, a right clik on the signal and then:
  Insert Analog Height Extension
  
- Scroll horizontally in the Waves pane to reach the useful samples
- Clik in the keyboard Ctrl + mousse scroll for a dynamic zoom

...
Close GTK Wave

6. Check and valid results
  cd ../../simulation/simu_fft_radix2_dit
  ./fft_radix2_dit_validation.py
  

