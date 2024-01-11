#!/usr/local/bin/perl -w

use lib "/opt/phnix/lib/perl5/site_perl/";
use strict;
#use Env;
#use Sys::mHostname;
#use DBI;

my $table = $ARGV[0];
my $infile = $ARGV[1];
my $type = $ARGV[2];

if ($#ARGV != 2) 
{
    print "To run this script type: \n";
    print "     ./check_calibrations_exist.pl [tablename] [infile] [type] \n";
    print "\n";
    print "where [table] is a table in the calibrations database \n";
    print "  the allowable svx calibration tables are:  svxpixelhotdeadchipmap, svxhotdeadstriphybrids and calibsvxbeamcenter \n";
    print "      [infile] is a text file that contains a list of run numbers you want to check with each runnumber on it's own line \n";
    print "      [type] is a unique identifier for the output file names \n";
    print "\n";
    print "This script will get the run2time start values for the runnumbers you list, run2time_[type].txt \n";
    print "It will get the start and end times of the calibrations in the database table [table], db_file_[table]_[type].txt \n";
    print "It will give the list of runs that need to be calibrated, neededruns_[table]_[type].txt \n";
    print "It will give the list of runs that are calibrated, calibruns_[table]_[type].txt \n";       
    exit;
}

if (!(-x "getManyStartTime.csh" ) || !(-x "getStartTime.csh") || !(-e "getManyStartTime.csh" ) || !(-e "getStartTime.csh") )
{
    print "To run this script you also need the scripts getManyStartTime.csh and getStartTime.csh \n";
    print "These are in cvs in the same directory as this script. \n";
    print "They must be executable, so chmod them appropriately. \n";
    exit;
}

if (!(-r $infile) || !(-f $infile) || !(-s $infile))
{
    print "$infile is not readable, is not a textfile, is empty, or doesn't exist \n";
    print "please use an appropriate input file \n";
    print "      [infile] is a text file that contains a list of run numbers you want to check with each runnumber on it's own line \n";
    exit;
}

if (!($table eq "svxpixelhotdeadchipmap" ) && !($table eq "svxhotdeadstriphybrids" ) && !($table eq "calibsvxbeamcenter"))
{
    print "$table is not one of the allowable calibrations tables. \n";
    print "      [table] is a table in the calibrations database \n";
    print "  the allowable svx calibration tables are:  svxpixelhotdeadchipmap, svxhotdeadstriphybrids and calibsvxbeamcenter \n";
    exit;
}

#define files and time constants
my $runfile = "run2time_${type}.txt";
my $outfile = ">neededruns_${table}_${type}.txt";
my $goodfile = ">calibruns_${table}_${type}.txt";
my $dbfile = "db_file_${table}_${type}.txt";

my $EndofTime = 2147483647;
my $FarFuture = 1343620800;
my $FarFuture2 = 1924923600;

#from run list get start times for each run
print "GET runnumber start times \n";
print "   you will see warnings: 'Warning in <TClassTable::Add>: class TSQLtatement already in TClassTable' \n";
print "   This happens, don't worry \n";

system("./getManyStartTime.csh $infile > $runfile");

if (!(-s $runfile))
{
    print "ERROR: for some reason the $runfile either doesn't exist or has size zero \n";
    print "cannot continue \n";
    exit
}

#open run file read runs,start times into arrays -- must be run ordered
print "READ runnumber start times \n";
my @RunList;
my @StartTimes;
my $i=0;
open (INF, $runfile) or die "could not open $runfile \n";
while (<INF>)
{
    my $line = $_;
    my @info = split(/\s+/,$line);
    $RunList[$i] = $info[0];
    $StartTimes[$i] = $info[1];
#    print "$info[0] $info[1] \n";
#    print "$i $StartTimes[$i] \n";
    $i++;
}
close(INF);

#make db file
print "GET database start and end times for table $table \n";
system("psql -h phnxdb2.phenix.bnl.gov calibrations -c  'select startvaltime, endvaltime from ${table} order by startvaltime' > ${dbfile}");
#system("psql -h phnxdb2.phenix.bnl.gov calibrations -c  'select startvaltime, endvaltime from ${table} order by startvaltime limit 10' > ${dbfile}"); #for testing

if (!(-s $dbfile))
{
    print "ERROR: for some reason the $dbfile either doesn't exist or has size zero \n";
    print "cannot continue \n";
    exit
}

#open db file read start,end times into arrays -- must be start time ordered
print "READ database start and end times \n";
my @DBStart;
my @DBEnd;
$i=0;
open (DBF, $dbfile) or die "could not open $dbfile \n";
while(<DBF>)
{
    my $line2 =$_;
    if ($line2 =~ "rows") { next; }
    if ($line2 =~ "time") { next; }
    my @info2 = split(/\s+/,$line2);
    if ($#info2 != 3) { next; } #print "info2 size wrong $#info2 \n"; }
    $DBStart[$i] = $info2[1];
    $DBEnd[$i] = $info2[3];
#    print "$info2[1] $info2[3] \n";
    $i++;    
}
close(DBF);

#check which runs are not in the db
print "COMPARE start and end times in $table with start times of the runs in $infile \n";
my $iprev=0;
$i--;
open (OUT, $outfile) or die "could not open $outfile \n";
open (GDF, $goodfile) or die "could not open $goodfile \n";
foreach my $index (0 .. $#RunList)
{
    my $search = 0;
    for (my $iloop = $iprev; $iloop < $#DBStart; $iloop++)
    {
	if ($DBEnd[$iloop] != $EndofTime && $DBEnd[$iloop] != $FarFuture && $DBEnd[$iloop] != $FarFuture2)
	{
	    if ($StartTimes[$index] >= $DBStart[$iloop] && $StartTimes[$index] < $DBEnd[$iloop])
	    {
		print GDF"$RunList[$index] \n"; 
#		print "for $RunList[$index] $StartTimes[$index]     found a match $DBStart[$iloop] $DBEnd[$iloop]\n";	
#		print "$RunList[$index] \n";
		$search = 1; #found a match
		$iprev = $iloop;
		$iloop = @DBStart;
	    }	
	}
	else
	{
	    if ($StartTimes[$index] == $DBStart[$iloop])
	    {
		print GDF"$RunList[$index] \n"; 
#		print "for $RunList[$index] $StartTimes[$index]     found a match $DBStart[$iloop] \n";
		$search = 1; #found a match
		$iprev = $iloop;
		$iloop = @DBStart;
	    }	
	}
    }
    if ($search == 0)
    {
	print OUT "$RunList[$index] \n";
#	print "for $RunList[$index]     NOT CALIBRATED \n";
    }
}
close(OUT);
close(GDF);

print "OUTPUT \n";
print "   calibrated runs:         $goodfile \n";
print "   not-calibrated runs:     $outfile \n";

exit;
