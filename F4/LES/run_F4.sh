stack build
rm out.log
F4-exe -m main.f95 -s ./src -o adam -o bondv1 -o feedbf -o les -o press -o velfg -o velnw -D WV_OPENCL,WV_NEW,WV_NEW_VELFG,NO_GLOBAL_SOR,INLINE_BOUND_CALCS,WV_TEST,TWINNED_BUFFER,NO_IO,IFBF=1,IADAM=0 >> out.log 2>&1
