#!/bin/csh
setenv DCACHE_DOOR phnxdoor1.rcf.bnl.gov:22133
setenv ODBCINI /opt/phenix/etc/odbc.ini.mirror

set prdf   = $1
set name   = `basename $prdf`
set out_dir = $2
@ nEvt = 0

set start_dir = $PWD

################################
# Copy PRDFF to condor scratch #
################################

cd $_CONDOR_SCRATCH_DIR
echo dccp $prdf .
dccp $prdf .

##########################
# Set Input/Output files #
##########################

echo ----INPUT FILE----
set input_file = $_CONDOR_SCRATCH_DIR"/"$name
echo $input_file

echo ----OUTPUT DIRECTORY----
echo $out_dir
mkdir -p $out_dir

cd $start_dir
#cd $out_dir
#ln -sf ${start_dir}"/"Fun4MisalignSearch.C .

############################
# Run Fun4MisalignSearch.C #
############################

echo ----RUNNING Fun4MisalignSearch.C----

#now run root
echo root -b -q Fun4MisalignSearch.C\($nEvt,\"$input_file\",\"$out_dir\"\)
root -b -q Fun4MisalignSearch.C\($nEvt,\"$input_file\",\"$out_dir\"\)


########################
# Now do some cleanup  #
########################

cd $start_dir
echo "---- DONE EXECUTING JOB "$nJob" ----"
