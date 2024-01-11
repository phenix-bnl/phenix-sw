#! /bin/csh -f 

source /opt/phenix/bin/phenix_setup.csh
foreach seg (0000)

setenv SEGMENT ${seg}

cd ${seg}
cp ../macros/*.* . 

bsub -o ../log/log_run_${seg}.log -e ../err/err_run_${seg}.log -q phenix_cas -R linux 'jobfile_test.csh'

unsetenv SEGMENT
cd ..
end

