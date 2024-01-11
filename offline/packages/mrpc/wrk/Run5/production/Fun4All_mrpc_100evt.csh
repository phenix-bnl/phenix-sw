#!/bin/csh -f

unsetenv OFFLINE_MAIN
source /opt/phenix/bin/phenix_setup.csh new
setenv LD_LOCAL /phenix/data20/chujo/MRPC/offline/install/lib
setenv LD_LIBRARY_PATH ${LD_LOCAL}:${LD_LIBRARY_PATH}

echo ${LD_LIBRARY_PATH}

setenv INPUTDIR /phenix/data20/chujo/PRDFF/Run5_CuCu_200GeV
setenv INFILE   EVENTDATAxxx_P01-0000151834-0000.PRDFF
#setenv INFILE   EVENTDATAxxx_P01-0000147564-0000.PRDFF
setenv PRDFNAME ${INPUTDIR}/${INFILE}

echo ${PRDFNAME}
echo ''
#########
echo '(( Fun4All -MRPC- ))'
root.exe -b <<EOF 
.x Fun4All_mrpc.C(100);
.q
EOF
