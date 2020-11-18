#!/usr/bin/env python3

"""
VHDL FFT mudule

FFT compare using python FFT built-in function
"""

#Standard python
import sys
import numpy

#Custom packages
sys.path.append('pytoolbox')
import pytoolbox.tools as mod_tools
import pytoolbox.spectrum as mod_spectrum
import pytoolbox.figure as mod_figure

class Const:
    PICKE_OBJ_FILE = "pickle_fft_radix2_dit_test_vector" #from fft_radix2_dit_test_vector.py
    FFT_VHDL_FILE = "../../fft_radix2_dit/tb/fft_vhdl.txt"
    PLOT = "SUBPLOT"
    AXIS_DEF = {
        "FREQ_HZ"    : {
            "format"    : 1,
            "unit"     :"Hz"
        },
        "FREQ_KHZ"    : {
            "format"    : 10**3,
            "unit"     :"kHz"
        }
    }
    
def main():
    """
    main function
    """
    #Check VHDL file
    tools = mod_tools.Tools()
    if (not tools.check_file_path(Const.FFT_VHDL_FILE)):
        raise Exception("The specified file" + " \"" + Const.FFT_VHDL_FILE + "\" " + 'not found.')
        
    #Read VHDL real/imag samples and do the shift here as it's not done on the VHDL output
    (fft_vhdl_real, fft_vhdl_imag) = tools.read_file_two_col_decimal(Const.FFT_VHDL_FILE)
    fft_vhdl_real = numpy.fft.fftshift(fft_vhdl_real)
    fft_vhdl_imag = numpy.fft.fftshift(fft_vhdl_imag)
    
    #Compute PSD of the VHDL complex FFT
    spectrum = mod_spectrum.Spectrum()
    fft_vhdl_cplx = numpy.array(fft_vhdl_real) + 1j*numpy.array(fft_vhdl_imag);
    (fft_vhdl_psd, pwr_unit) = spectrum.psd_db(fft_vhdl_cplx)
        
    #Load test config
    config = tools.load_obj_unpickle(Const.PICKE_OBJ_FILE)
    Fs = config["Fs"]
    n_samp = config["n_samp"]

    #Read PYTHON generated test vectors
    fft_python_real = tools.read_file_one_col_decimal(config["python_re_file"])
    fft_python_imag = tools.read_file_one_col_decimal(config["python_im_file"])
    fft_python_psd = tools.read_file_one_col_decimal(config["python_psd_file"])
    
    #Diffs, clip to 1 values <1 in order avoid negative log and have 0 instead
    delta_re = numpy.absolute(fft_vhdl_real - fft_python_real)
    delta_re = numpy.clip(delta_re, 1, None)
    delta_re = numpy.log2(delta_re)
    
    delta_im = numpy.absolute(fft_vhdl_imag - fft_python_imag)
    delta_im = numpy.clip(delta_im, 1, None)
    delta_im = numpy.log2(delta_im)
    
    delta_psd = numpy.absolute(fft_vhdl_psd - fft_python_psd)
    delta_psd = numpy.clip(delta_psd, 1, None)
    delta_psd = numpy.log2(delta_psd)
    
    #Plot
    (f_axis, f_unit) = spectrum.build_axis_freq(Fs, n_samp, Const.AXIS_DEF["FREQ_KHZ"])
    
    if (Const.PLOT == "SUBPLOTxx"):
        curve = mod_figure.Plot(1, show_after = 6)
        curve.subplot(321, f_axis, fft_vhdl_real, f_unit, "Amplitude", "FFT VHDL real samples")
        curve.subplot(322, f_axis, fft_vhdl_imag, f_unit, "Amplitude", "FFT VHDL imag samples")
        curve.subplot(323, f_axis, fft_python_real, f_unit, "Amplitude", "FFT PYTHON real samples")
        curve.subplot(324, f_axis, fft_python_imag, f_unit, "Amplitude", "FFT PYTHON imag samples")
        curve.subplot(325, f_axis, fft_vhdl_psd, f_unit, pwr_unit, "FFT VHDL PSD")
        curve.subplot(326, f_axis, fft_python_psd, f_unit, pwr_unit, "FFT PYTHON PSD")
    else:
        curve = mod_figure.Plot(show_after = 4)
        curve.multiplot(1, [f_axis, fft_vhdl_real, 'r--', f_axis, fft_python_real, 'b--', 'VHDL', 'PYTHON'], f_unit, "Amplitude", "FFT VHDL vs PYTHON real samples")
        curve.multiplot(2, [f_axis, fft_vhdl_imag, 'r--', f_axis, fft_python_imag, 'b--', 'VHDL', 'PYTHON'], f_unit, "Amplitude", "FFT VHDL vs PYTHON imag samples")
        curve.multiplot(3, [f_axis, fft_vhdl_psd, 'r--', f_axis, fft_python_psd, 'b--', 'VHDL', 'PYTHON'], f_unit, pwr_unit, "FFT VHDL vs PYTHON psd samples")
        curve.multiplot(4, [f_axis,delta_re, 'r--', f_axis, delta_im, 'b--', f_axis, delta_psd, 'g--', 'Real diff', 'Imag diff', 'PSD diff'], f_unit, "bits", "Quantization noise")
                                       
if __name__ == "__main__":
    main()
    
