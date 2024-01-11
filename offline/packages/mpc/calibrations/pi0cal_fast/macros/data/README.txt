Beau Meredith 11-11-2010

These are the macros used in first step used in the iterative pi0calib.  This step has
no iteration, but writes out a TTree with cluster pairs that pass some
minimal set of cuts.  This is the only portion of the code that
requires fun4all.  

The macro runs with the command 

perl runmpc.pl <file_index> <nevts>

e.g. to run the first file in the list for 1k events, just do 

cd macros
perl runmpc.pl 0 1000

An important file in this setup is called fullpathsync.list This is a
file containing a list of list files (e.g. if you need 2 DST types,
fullpathsync.list contains the path to both list files).  This needs
to be modified for every run, but if you contact me at
beau.meredith@gmail.com, I can easily set this up for you.

This runs the macro Run_mpc.C
The output will show up in the directory called condor_output

To run many jobs there is a script file called runmpc.csh (this just
runs runmpc.pl with a fixed number of events) and a job file called
runmpc.job.  Just choose how many jobs you want to run afte Queue and
do 

condor_submit runmpc.job.


