Beau Meredith 11-11-2010

This is the first step used in the iterative pi0calib.  This step has
no iteration, but writes out a TTree with cluster pairs that pass some
minimal set of cuts.  This is the only portion of the code that
requires fun4all.  

To run this code on some sample run8 p+p files go to ./macros/data.

To run it on p+p simulation files go to ./macros/sim

The macro runs with the command 

perl runmpc.pl <file_index> <nevts>

e.g. to run the first file in the list for 1k events, just do 
cd macros
perl runmpc.pl 0 1000

This runs the macro Run_mpc.C
The output will show up in the directory called condor_output

To run many jobs there is a script file called runmpc.csh (this just
runs runmpc.pl with a fixed number of events) and a job file called
runmpc.job.  Just choose how many jobs you want to run afte Queue and
do 

condor_submit runmpc.job.

In the sim directory the scripts that are named run_mpc_sim.xxx that
do the same thing for the simulation files
