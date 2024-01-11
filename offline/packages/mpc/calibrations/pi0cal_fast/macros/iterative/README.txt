Beau Meredith 11-11-10

This is the iterative part of the pi0 calibration.  It takes the
output from the class mpcPi0TTree and iteratively modifies the gains
to match the mass to results from p+p 200 GeV pythia simulatoin run
through GEANT.  The simulation file is ./sim_means_combined.txt.
Because of the changes to mpcmap, there is some possibility these
files should be slightly modified.


***********************************
RUNNING THE PI0CAL ON DATA
To run the pi0cal on data, just do

perl run_cal.pl <listname> <Nevents/iteration>

The list contains output from the mpcPi0TTree.

For example, I would do

perl run_cal.pl list_pi0_data.txt 5000000

This takes output from run8 p+p data and runs the calibration with 5M
evts per iteration.  A better number to use for 200 GeV p+p is 20M.

The output will be in the directory called recal.  This will have text
files, .pngs, and a .root file.  The text files tell one how the gain
changes; I used recal/gains_data_6 for the run8 dataset.  There is
also information on the mass and width that is spit out for each
iteration.  The root file contains all the mass histograms for all iterations.
***********************************


**********************************
GENERATING THE SIMULATION MASSES
To generate the simulation comparison located in ./recal_sim, run

perl run_cal_sim.pl <listname> <Nevts>

For example, I would do

perl run_cal_sim.pl list_pi0_sim.txt 5000000

The output is in recal_sim and includes a text file with the sim
masses, a root file containing the histograms, and a couple of .pngs

*************************