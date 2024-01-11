This code was written to correct the event mis-alignments from jump chips in the VTXP,
which became really prevalent in Run16.

It is organized into 3 Passes,

PASS 1: Copy out raw bad VTXP data from PRDF and info needed to find event mis-alignments

PASS 2: Take data needed to find event jumps, sort events, find jumps, store jump info in database

PASS 3: Use jump database to move VTXP raw data from wrong event to right event, write out slim PRDF that contains just good data

This should then just take one pass through the PRDF in HPSS.

Here is the code that does each pass:

PASS 1: The macro that reads through the PRDF is Fun4JumpChip.C, which executes SvxJumpChipReco.C. 
This step basically just calculates the nhits in the bad chips, and copies over all the info that
will be needed in the later passes.  An example script that runs the macro is in macros/runjumpchip.cmd
(which you may use if you like but is not required).  In it is an example of how to copy the PRDF
from dCache to the local CONDOR disk.  A bunch of vtxjump.root files are created in this pass.
** STILL NEED TO WRITE CODE THAT COPIES OVER RAW VTXP PACKET DATA **

PASS 2: Here we'll first need to sort the events in the vtxjump.root files, and write out a ttree with
all the info needed to find the event misalignments for each chip.  The event misalignments are stored
in a database for PASS 3.
** STILL need to create database, tune code to find event misalignments **

PASS 3: Read in the raw data from the vtxjump.root files, and write out corrected VTXP raw data as PRDF.
We find how much and whether to shift a chip's data from the event misalignment database, and we
find the event which has the data from the sorted.root file.
** THIS PART HAS NOT BEEN DONE YET AT ALL **
