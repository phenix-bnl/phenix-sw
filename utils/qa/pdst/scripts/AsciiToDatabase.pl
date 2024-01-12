#!/usr/bin/perl

#============================================================
# This script will read in a simple 3-column Ascii file and 
# submit the values into the QA Database.  Users will need to
# personalize (at minimum) steps 1-3 for their submissions.
#
#         Michael P. McCumber
#         mccumber@grad.physics.sunysb.edu
#         1/18/2005
#============================================================

#(1)---Name Your QA Parameter and Location Here---
$name = "AjitPairQa";
$table = "physicsqa";
$tag = "run4AuAu_Central_200GeV_v01_pro56";

#(2)---Tell the script where your values are stored---
#Local file must contain 3 columns in format:
# <runNumber> <value> <error>
open(FILE, "qalist_ajit.out");

#(3)---After testing the output, set to commit=1---
$commit = 1;
$verbosity = 1;

#Other database entries
$segment = -1;

#Getting entry time (is used later in database management
#only most recent entry for a given run# and parameter is used)
$time = time;

#Get the next line in the file
while($line = <FILE>) {

    #Extract the data
    @data = split ' ', $line;
    $data[0] = int $data[0];

    #Return the command to screen - sanity check
    if ($verbosity == 1) {
	system ("echo The command:");
	system ("echo "."psql calibrations -c \"insert into ".$table." values(".$data[0].",".$segment.",'".$tag."',".$time.",".$data[1].",".$data[2].",'".$name."')\"");
    }

    #Submit to the database
    if ($commit != 1) {
	system ("echo was not submitted as commit is not set to 1.");
    }

    if ($commit == 1) {
	system ("psql calibrations -c \"insert into ".$table." values(".$data[0].",".$segment.",'".$tag."',".$time.",".$data[1].",".$data[2].",'".$name."')\"");
	if ($verbosity == 1) {
	    system ("echo has been submitted.");
	}
    }

    
}

close FILE;
exit 0;
