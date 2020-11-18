#!/usr/bin/env python3

"""
test vector mudule

Generates test vector for VHDL tb
"""

#Standard python
import sys
import numpy

#Custom packages
sys.path.append('pytoolbox')
import pytoolbox.tools as mod_tools
import pytoolbox.spectrum as mod_spectrum
import pytoolbox.figure as mod_figure
import pytoolbox.noise as mod_noise

class Const:
    TIME_REAL_FILE = "../../fft_radix2_dit/tb/time_signal_real.txt"
    TIME_IMAG_FILE = "../../fft_radix2_dit/tb/time_signal_imag.txt"
    FFT_REAL_FILE = "fft_python_real.txt"
    FFT_IMAG_FILE = "fft_python_imag.txt"
    FFT_PSD_FILE = "fft_python_psd.txt"
    PLOT = "SUBPLOT"
    AXIS_DEF = {   
        "TIME_S"    : {
            "format"    : 1,
            "unit"     :"s"
        }, 
        "TIME_MS"   : {
            "format"    : 10**(-3),
            "unit"     :"ms"
        },
        "FREQ_HZ"    : {
            "format"    : 1,
            "unit"     :"Hz"
        },
        "FREQ_KHZ"    : {
            "format"    : 10**3,
            "unit"     :"kHz"
        }
    }
    SAMPLING_FREQ = 100e3 #Hz
    NOF_SAMPLES = 256
    CARRIER_FREQ = 35e3 #Hz
    ENBW = CARRIER_FREQ/2
    QUANT = 12 #"ADC" quantization
    SNR_DB = 30
    PICKE_OBJ_FILE = "pickle_fft_radix2_dit_test_vector"
    PICKLE_CONFIG = {
        "Fs": SAMPLING_FREQ,
        "n_samp": NOF_SAMPLES,
        "python_re_file": FFT_REAL_FILE,
        "python_im_file": FFT_IMAG_FILE,
        "python_psd_file": FFT_PSD_FILE
    }
           
def main():
    """
    main function
    """
    
    #Signal parameters 
    tools = mod_tools.Tools()
    tools.dump_obj_pickle(Const.PICKE_OBJ_FILE, Const.PICKLE_CONFIG)
    
    #Add noise
    noise = mod_noise.Noise()
    exp_cplx = noise.build_exp_cplx(Const.NOF_SAMPLES, Const.SAMPLING_FREQ, Const.CARRIER_FREQ)
    (s_n, real_samp, imag_samp) = noise.add_noise_cplx(exp_cplx, Const.NOF_SAMPLES, Const.SAMPLING_FREQ, Const.SNR_DB, Const.ENBW, Const.QUANT)
    
    #Spectrum
    spectrum = mod_spectrum.Spectrum()
    spectrum_cplx = spectrum.fft(s_n, Const.NOF_SAMPLES)
    (spectrum_psd, pwr_unit) = spectrum.psd_db(spectrum_cplx)
    
    #Save files
    tools.write_file_one_col_decimal(Const.TIME_REAL_FILE, numpy.real(s_n))
    tools.write_file_one_col_decimal(Const.TIME_IMAG_FILE, numpy.imag(s_n))
    tools.write_file_one_col_decimal(Const.FFT_REAL_FILE, numpy.real(spectrum_cplx))
    tools.write_file_one_col_decimal(Const.FFT_IMAG_FILE, numpy.imag(spectrum_cplx))
    tools.write_file_one_col_decimal(Const.FFT_PSD_FILE, spectrum_psd)
    
    #Plot
    (t_axis, t_unit, f_axis, f_unit) = spectrum.build_axis_tf(Const.SAMPLING_FREQ, Const.NOF_SAMPLES, Const.AXIS_DEF["TIME_MS"], Const.AXIS_DEF["FREQ_KHZ"])
    
    if (Const.PLOT == "SUBPLOT"):
        curve = mod_figure.Plot(1, show_after = 3)
        curve.subplot(311, t_axis, real_samp, t_unit, "Amplitude", "Real samples")
        curve.subplot(312, t_axis, imag_samp, t_unit, "Amplitude", "Imaginary samples")
        curve.subplot(313, f_axis, spectrum_psd, f_unit, pwr_unit, "PSD")
    else:
        curve = mod_figure.Plot(show_after = 3)
        curve.plot(1, t_axis, real_samp, t_unit, "Amplitude", "Left channel")
        curve.plot(2, t_axis, imag_samp, t_unit, "Amplitude", "Right channel")
        curve.plot(3, f_axis, spectrum_psd, f_unit, pwr_unit, "Left channel FFT")
    
                                       
if __name__ == "__main__":
    main()
    
