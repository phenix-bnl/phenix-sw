#!/usr/local/bin/perl
##########################
use DBI;
use DBD::Pg qw(:pg_types);
use Getopt::Long;
#command line options
GetOptions( "verbose"=>\$verbose,
            "iterations=i"=>\$itterations,
	    "noutfile=s"=>\$noutfile,
	    "soutfile=s"=>\$soutfile,
            "help"=>\$help,
	    "mean"=>\$mean);

&help if($help); 
&help if( !$itterations && !$mean);
$outfile1="good_runs_north.txt" if (! $noutfile );
$outfile2="good_runs_south.txt" if (! $soutfile );
#database stuff
$dbuser="phnxrc";
$dbhost="phnxdb2.phenix.bnl.gov";
$dbname="calibrations";

#####################BEGIN of THe Monster HASH##############################
my %qaList = (
      HV => {
		  North => 
		  {
		      parameters => 1,
		      parnames => ["North:  No. HV Disabled"] ,
		      outfile => "badHVNorth.txt",
		      mean => [0],
#		      sigma => [37.5], #changed north to 100
		      sigma => [50],
		      lastBadRun => -9999,
		  },
		  South => 
		  {
		      parameters => 1,
		      parnames => ["South:  No. HV Disabled"] ,
		      outfile => "badHVSouth.txt",
		      mean => [0],
		      sigma => [37.5],
		      lastBadRun => -9999,
		  },
		  Table => "mutrqa",
	      },
	      CC => {
		  North =>
		  {
		      parameters => 3,
		      parnames => ["North:  <Fitted Cath.Cluster> S1", "North:  <Fitted Cath.Cluster> S2", "North:  <Fitted Cath.Cluster> S3" ] ,
		      outfile => "badClustersNorth.txt",
		      mean => [0,0,0],
		      sigma => [0,0,0],
		      lastBadRun => -9999,
		  },
		  South => 
		  {
		      parameters => 3,
		      parnames => ["South:  <Fitted Cath.Cluster> S1", "South:  <Fitted Cath.Cluster> S2", "South:  <Fitted Cath.Cluster> S3" ] ,
		      outfile => "badClustersSouth.txt",
		      mean => [0,0,0],
		      sigma => [0,0,0],
		      lastBadRun => -9999,
		  },
		  Table => "mutrqa",
	      },
	      CPQ => {
		  North =>
		  {
		      parameters => 1,
		      parnames => ["North:  <Cath.Cluster Peak Charge>" ] ,
		      outfile => "badPeakQNorth.txt",
		      mean => [0],
		      sigma => [0],
		      lastBadRun => -9999,
		  },
		  South => 
		  {
		      parameters => 1,
		      parnames => ["South:  <Cath.Cluster Peak Charge>" ] ,
		      outfile => "badPeakQSouth.txt",
		      mean => [0],
		      sigma => [0],
		      lastBadRun => -9999,
		  },
		  Table => "mutrqa",
	      },
	      HPE => {
		  North =>
		  {
		      parameters => 3,
		      parnames => ["North:  No. Hits/event/plane S1", "North:  No. Hits/event/plane S2", "North:  No. Hits/event/plane S3"] ,
		      outfile => "lowHitsNorth.txt",
		      mean => [1,1,1],
		      sigma => [0,0,0],
		      lastBadRun => -9999,
		  },
		  South => 
		  {
		      parameters => 3,
		      parnames => ["South:  No. Hits/event/plane S1", "South:  No. Hits/event/plane S2", "South:  No. Hits/event/plane S3"] ,
		      outfile => "lowHitsSouth.txt",
		      mean => [1,1,1],
		      sigma => [0,0,0],
		      lastBadRun => -9999,
		  },
		  Table => "mutrqa",
	      },
	      HP => {
		  North =>
		  {
		      parameters => 1,
		      parnames => ["North:  No. Hot Packets"] ,
		      outfile => "hotPacketNorth.txt",
		      mean => [0],
		      sigma => [5],
		      lastBadRun => -9999,
		  },
		  South => 
		  {
		      parameters => 1,
		      parnames => ["North:  No. Hot Packets"] ,
		      outfile => "hotPacketSouth.txt",
		      mean => [0],
		      sigma => [5],
		      lastBadRun => -9999,
		  },
		  Table => "mutrqa",
	      },
	      HPL => {
		  North =>
		  {
		      parameters => 3,
		      parnames => ["North:  No. Hot Plane S1","North:  No. Hot Plane S2","North:  No. Hot Plane S3"] ,
		      outfile => "hotPlaneNorth.txt",
		      mean => [0,0,0],
		      sigma => [5,5,5],
		      lastBadRun => -9999,
		  },
		  South => 
		  {
		      parameters => 3,
		      parnames => ["South:  No. Hot Plane S1","South:  No. Hot Plane S2","South:  No. Hot Plane S3"] ,
		      outfile => "hotPlaneSouth.txt",
		      mean => [0,0,0],
		      sigma => [5,5,5],
		      lastBadRun => -9999,
		  },
		  Table => "mutrqa",
	      },
	      DP => {
		  North =>
		  {
		      parameters => 1,
		      parnames => ["North:  No. Dead Packets"] ,
		      outfile => "disabledPacketNorth.txt",
		      mean => [0],
		      sigma => [5],
		      lastBadRun => -9999,
		  },
		  South => 
		  {
		      parameters => 1,
		      parnames => ["South:  No. Dead Packets"] ,
		      outfile => "disabledPacketSouth.txt",
		      mean => [0],
		      sigma => [5],
		      lastBadRun => -9999,
		  },
		  Table => "mutrqa",
	      },
	      DPL => {
		  North =>
		  {
		      parameters => 3,
		      parnames => ["North:  No. Dead Plane S1","North:  No. Dead Plane S2","North:  No. Dead Plane S3"] ,
		      outfile => "disabledPlanesNorth.txt",
		      mean => [0,0,0],
		      sigma => [5,5,5],
		      lastBadRun => -9999,
		  },
		  South => 
		  {
		      parameters => 3,
		      parnames => ["South:  No. Dead Plane S1","South:  No. Dead Plane S2","South:  No. Dead Plane S3"] ,
		      outfile => "disabledPlanesSouth.txt",
		      mean => [0,0,0],
		      sigma => [5,5,5],
		      lastBadRun => -9999,
		  },
		  Table => "mutrqa",
	      },
	      );
#####################END of THe MoNster HASH################################
#get_avg and the main loop need to be able to take file lists as the input as well so we can itterate.
$tag='run7AuAu_Muon_200GeV_pro78_new';
sub get_avg;
$dbh = DBI->connect("dbi:Pg:dbname=$dbname;host=$dbhost", "$dbuser", "") or die $DBI::errstr;
my @testBadRuns=();
my @goodRunsNorth=();
my @goodRunsSouth=();
#first load up the averages with no bad runs
get_avg(\@testBadRuns, \%qaList);
die("Only Calculating the mean and std deviation") if ($mean);
#now get a run list 
$command="SELECT distinct(runnumber) FROM $qaList{CC}{Table} where tag='${tag}' and parname like '%Hot Plane%' and tag='${tag}'";

print "$command\n" if ($verbose);

$sth = $dbh->prepare("$command");

$sth->execute( );

#now find and throw out the furthest runnumber for each parameter.
while ( @row = $sth->fetchrow_array ) 
{
    my $runnumber = $row[0];
    unshift(@goodRunsNorth,$row[0]);
    unshift(@goodRunsSouth,$row[0]);
    print "Checking QA for $runnumber...\n";
    for $qaName (keys %qaList) 
    {
	for $arm ( keys %{$qaList{$qaName}} )
	{
	    if ( $arm=~/Table/ )
	    {
		next;
	    }
	    for ($ipar=0; $ipar < $qaList{$qaName}{$arm}{parameters}; $ipar++ )
	    {
		if ( $qaList{$qaName}{$arm}{mean}[$ipar] && $qaList{$qaName}{$arm}{sigma}[$ipar] )
		{ 
		    print "$qaName $arm $runnumber\n" if ($verbose);
		    $command="SELECT (parameter-$qaList{$qaName}{$arm}{mean}[$ipar])/$qaList{$qaName}{$arm}{sigma}[$ipar] from  $qaList{CC}{Table} where parname='$qaList{$qaName}{$arm}{parnames}[$ipar]' and runnumber=$runnumber and tag='${tag}'";
		    
		    print "$command\n" if ($verbose);
		    $sth2 = $dbh->prepare("$command");
		    
		    $sth2->execute();

		    while ( @row2 = $sth2->fetchrow_array ) 
		    {
			print "std dev: @row2\n" if ($verbose);
			if ( abs($row2[0]) > 2 )
			{
			    print "$runnumber is $row2[0] away from mean for $qaList{$qaName}{$arm}{parnames}[$ipar]\n";
			    if ($runnumber != $qaList{$qaName}{$arm}{lastBadRun} )
			    {
				$qaList{$qaName}{$arm}{lastBadRun}=$runnumber;
				open MYFILE, ">>$qaList{$qaName}{$arm}{outfile}" || die ("Could not open output file $qaList{$qaName}{$arm}{outfile}");
				print MYFILE "$runnumber\n";
				close ( MYFILE );
			    }
			    shift(@goodRunsNorth) if ( $runnumber == $goodRunsNorth[0] && $arm=~/North/ );
			    shift(@goodRunsSouth) if ( $runnumber == $goodRunsSouth[0] && $arm=~/South/ );
				
			}

		    }
		}
		elsif ( !$qaList{$qaName}{$arm}{mean}[$ipar] )
		{
		    #these are the ones that have to be less than 10
		    #so we will just see if they are less than or more
		    #than 2sigma....
		    print "$qaName $arm $runnumber\n" if ($verbose);
		    $command="SELECT parameter from  $qaList{CC}{Table} where parname='$qaList{$qaName}{$arm}{parnames}[$ipar]' and runnumber=$runnumber and tag='${tag}'";
		    
		    print "$command\n" if ($verbose);
		    $sth2 = $dbh->prepare("$command");
		    
		    $sth2->execute();

		    while ( @row2 = $sth2->fetchrow_array ) 
		    {
			print "std dev: @row2\n" if ($verbose);
			if ( $row2[0] > 2*$qaList{$qaName}{$arm}{sigma}[$ipar] )
			{
			    print "$runnumber is $row2[0] > 2 X $qaList{$qaName}{$arm}{sigma}[$ipar] for $qaList{$qaName}{$arm}{parnames}[$ipar]\n";
			    if ($runnumber != $qaList{$qaName}{$arm}{lastBadRun} )
			    {
				$qaList{$qaName}{$arm}{lastBadRun}=$runnumber;
				open MYFILE, ">>$qaList{$qaName}{$arm}{outfile}" || die ("Could not open output file $qaList{$qaName}{$arm}{outfile}");
				print MYFILE "$runnumber\n";
				close ( MYFILE );
			    }
			    shift(@goodRunsNorth) if ( $runnumber == $goodRunsNorth[0] && $arm=~/North/ );
			    shift(@goodRunsSouth) if ( $runnumber == $goodRunsSouth[0] && $arm=~/South/ );

			}
		    }
		
		}
		elsif ( !$qaList{$qaName}{$arm}{sigma}[$ipar] )
		{
		    #these are the ones that have to be greater than
		    #the mean which is most lkikely 1.
		    print "$qaName $arm $runnumber\n" if ($verbose);
		    $command="SELECT parameter from  $qaList{CC}{Table} where parname='$qaList{$qaName}{$arm}{parnames}[$ipar]' and runnumber=$runnumber and tag='${tag}'";
		    
		    print "$command\n" if ($verbose);
		    $sth2 = $dbh->prepare("$command");
		    
		    $sth2->execute();

		    while ( @row2 = $sth2->fetchrow_array ) 
		    {
			print "std dev: @row2\n" if ($verbose);
			if ( $row2[0] < $qaList{$qaName}{$arm}{mean}[$ipar] )
			{
			    print "$runnumber is $row2[0] < $qaList{$qaName}{$arm}{mean}[$ipar] for $qaList{$qaName}{$arm}{parnames}[$ipar]\n";
			    if ($runnumber != $qaList{$qaName}{$arm}{lastBadRun} )
			    {
				$qaList{$qaName}{$arm}{lastBadRun}=$runnumber;
				open MYFILE, ">>$qaList{$qaName}{$arm}{outfile}" || die ("Could not open output file $qaList{$qaName}{$arm}{outfile}");
				print MYFILE "$runnumber\n";
				close ( MYFILE );
			    }
			    shift(@goodRunsNorth) if ( $runnumber == $goodRunsNorth[0] && $arm=~/North/ );
			    shift(@goodRunsSouth) if ( $runnumber == $goodRunsSouth[0] && $arm=~/South/ );
			}
		    }
		    
		}
	    }
	}
    }
}

#now write out the good runs in our list
open MYFILE, ">$noutfile" || die ("Could not open output file $noutfile");
foreach (@goodRunsNorth)
{
    print MYFILE "$_\n";
}
close ( MYFILE );
open MYFILE, ">$soutfile" || die ("Could not open output file $soutfile");
foreach (@goodRunsSouth)
{
    print MYFILE "$_\n";
}
close ( MYFILE );
$dbh->disconnect;
#####################SUBROUTINES############################################
sub get_avg()
{
    
    my ( $runs, $qlist ) = @_;
    foreach (@$runs)
    {
	$badruns="and runnumber!=$_ $badruns";
    }
    
   #The loop to calculate the values    
    for $qaName (keys %$qlist) 
    {
	for $arm ( keys %{$qlist->{$qaName}} )
	{
	    if ( $arm=~/Table/ )
	    {
		next;
	    }
	    for ($ipar=0; $ipar < $qlist->{$qaName}{$arm}{parameters}; $ipar++ )
	    {
		if ( !$qlist->{$qaName}{$arm}{mean}[$ipar] && !$qlist->{$qaName}{$arm}{sigma}[$ipar] )
		{
		    print "call psql to calculate mean/sigma for $qlist->{$qaName}{$arm}{parnames}[$ipar]\n" if ($verbose);
		    my $command="SELECT sum(parameter)/count(parameter) FROM $qlist->{$qaName}{Table} WHERE parname='$qlist->{$qaName}{$arm}{parnames}[$ipar]' $badruns and tag='${tag}'";
		    print "$command\n" if ($verbose);
		    $sth = $dbh->prepare("$command");
		    
		    $sth->execute( );
		    
		    while ( @row = $sth->fetchrow_array ) {
			print "@row\n" if ($verbose);
			$qlist->{$qaName}{$arm}{mean}[$ipar]=$row[0];
		    }


		    $command="SELECT sum(pow(parameter-$qlist->{$qaName}{$arm}{mean}[$ipar],2))/count(parameter) FROM $qlist->{$qaName}{Table} WHERE parname='$qlist->{$qaName}{$arm}{parnames}[$ipar]' $badruns and tag='${tag}'";
		    print "$command\n" if ($verbose);
		    $sth = $dbh->prepare("$command");
		    
		    $sth->execute( );
		    
		    while ( @row = $sth->fetchrow_array ) {
			$qlist->{$qaName}{$arm}{sigma}[$ipar]=sqrt($row[0]);
		    }
		    print "${qaName}\[${ipar}\]:$qlist->{$qaName}{$arm}{parnames}[$ipar] $qlist->{$qaName}{$arm}{mean}[$ipar] +- $qlist->{$qaName}{$arm}{sigma}[$ipar] \n" if ($verbose || $mean);
		}
	    }
	}
    }
}
#Now loop over all the runs and make the decision make the decisions
#and print the lists

sub help()
{
    print "USAGE: make_bad_run_lists.pl -i <itterations>\n\n";
    print "       -h, --help               Print this help\n";             
    print "       -i, --itterations <itterations> Set the number of itterations\n";
    print "       -n, --noutfile <outfile>     Set the north outfile name\n";
    print "       -s, --soutfile <outfile>     Set the south outfile name\n";
    print "       -m, --mean     Only calculate the average and standard deviation\n";
    exit;
}

