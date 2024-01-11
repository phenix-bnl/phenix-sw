#! /usr/bin/perl

###############################################################
# $Id: parse_hv_db.pl,v 1.2 2009/07/14 21:32:01 phnxreco Exp $
# retrieves number of tripped HV for a given run, using folowing databases
# 1/ run: to get good run list (otherwise specified in run list input file), as well as begin-of-run and end-of-run timestamps
# 2/ hvlog_run: to get list of channels that are tripped *at the beginning* of the run
# 3/ hv_triplog: to get list of channels that trip during the run.
use Getopt::Long;
use strict;
use Env;
use POSIX;
use DBI;
#use Time::localtime;

##################################
# Main

## prepare logfile
my $logfile = "parse_hv_db.log";
print( "parse_hv_db - writting log to $logfile\n" );
open(LOG, ">$logfile");

## prepare the daq DB
my $daq = DBI->connect("dbi:ODBC:daq") || die $DBI::error;

# retrieve options
print_and_log( "parse_hv_db.pl" );
  
my ($opt_runlist,$opt_first_run,$opt_last_run,$opt_output_path,$opt_help);
GetOptions(
	   'runlist:s' => \$opt_runlist,
	   'first-run:s' => \$opt_first_run,
	   'last-run:s' => \$opt_last_run,
	   'output-path:s' => \$opt_output_path,
	   'help' => \$opt_help
	   );
  
# check usage
if( $opt_help )
{
  usage();
  exit;
}    
  
my %selected_runs;
my $first_run;
my $last_run;
if( $opt_runlist )
{

  my $runlist = $opt_runlist;
  print_and_log( "run list: $runlist" );

  # get list of runs from input file
  my @runs = sort( split(  /\s*\n/, `more $runlist` ) );
  print( "parse_hv_db - input_runs: @runs" );

  # first run
  $first_run = $runs[0];
  $last_run = $runs[-1];

  # copy list of runs into hash table
  @selected_runs{@runs}=();

} 
else 
{

  # retrieve first and last run
  if( !$opt_first_run )
  {
    print_and_log( "parse_hv_db - first run not specified" );
    usage();
    exit;
  }

  $first_run = $opt_first_run;
  $last_run = $opt_last_run;

}

print_and_log( "parse_hv_db - first run: $first_run" );
print_and_log( "parse_hv_db - last run: $last_run" );

my $output_path = ".";
if( $opt_output_path ) { $output_path = $opt_output_path; }
print_and_log( "parse_hv_db - output path: $output_path" );

# psql row selection
my $rows = "Run.RunNumber,Run.BRUnixTime,Run.ERUnixTime";

# run selection
my $run_selection = "";
if( $last_run ) { $run_selection = "Run.RunNumber>=$first_run AND Run.RunNumber<=$last_run"; }
else { $run_selection = "Run.RunNumber>=$first_run"; }

# additional selections
my $selection = "$run_selection AND Run.RunType=\'PHYSICS\'";

# generate psql command
my $psql_command = "select $rows FROM run WHERE $selection";
my $get_runs = $daq->prepare($psql_command);
$get_runs->execute();
print_and_log( "parse_hv_db::_select_runs - psql_command: $psql_command" );
print_and_log( "" );

my $size = $get_runs->rows;
print_and_log( "parse_hv_db - found $size lines" );

my( $run,$begin_unix,$end_unix );
$get_runs->bind_col(1, \$run);
$get_runs->bind_col(2, \$begin_unix);
$get_runs->bind_col(3, \$end_unix);

while ($get_runs->fetch) {
  if( $opt_runlist && !(exists $selected_runs{$run}) ) { print_and_log( "run $run discarded." ); }
  else { _process_line( $run,$begin_unix,$end_unix ); }
}
$get_runs->finish();
  
print_and_log( "parse_hv_db - completed." );
close( LOG );
$daq->disconnect;
  
exit; 

###########################################
# process each output line from psql
sub _process_line
{
  # get run number
  my $run = $_[0];
  my $begin_unix = $_[1];
  my $end_unix=$_[2]; 
  
  if( $run == "" ) { 
    print_and_log( "parse_hv_db::_process_line - no runnumber for begin: $begin_unix, end: $end_unix" ); 
    return; 
  }
   
  print_and_log( "\nparse_hv_db::_process_line - run $run" );
  
  # get list of disabled channels at beginning of run.
  my %disabled_channels = _get_disabled_channels( $run );
  
  # print list of channels to log file
  foreach my $channel ( keys( %disabled_channels ) )
  {
    
    # skip channel if associated trip counts is zero (should not happen)
    next if( !$disabled_channels{$channel} );

    # format is: run number, channel, channel trip time (here begin of run), run begin time, run end time, first measurement in run (here begin of run)
    print_and_log( "parse_hv_db::trip "
      ."$run"
      ." $channel"
      ." $begin_unix" 
      ." $begin_unix $end_unix $begin_unix" ); 
    
  }

  # add list of channels that tripped during the run
  my %new_disabled_channels = _get_new_disabled_channels( $run, $begin_unix, $end_unix );
  my $new_channels_count = 0;
  my $new_channels_count_south = 0;
  my $new_channels_count_north = 0;
  my $runtime = $end_unix - $begin_unix;

  foreach my $channel ( keys( %new_disabled_channels ) )
  {
    
    # skip channel if associated trip time is zero
    # this usually corresponds to channel that are "recovered" during runs, which, in turn is characteristic
    # for a fake trip
    next if( !$new_disabled_channels{$channel} );
    
    # chip if channel is already in list of channels that are tripped at the beginning of the run
    next if( $disabled_channels{$channel} );
    
    print_and_log( "parse_hv_db::new_trip "
      ."$run"
      ." $channel"
      ." $new_disabled_channels{$channel}"
      ." $begin_unix $end_unix $begin_unix" ); 

    $new_channels_count ++;
    
    if( $channel =~ /MUID_HV_N\d+/ ) { $new_channels_count_north++; }
    elsif( $channel =~ /MUID_HV_S\d+/ ) { $new_channels_count_south++; }
    
    $disabled_channels{$channel}++;
    
  }

  print_and_log( "parse_hv_db::_process_line - run $run new channels south: $new_channels_count_south north: $new_channels_count_north" );
  
  # counts disabled channels for each arm
  my $size = keys( %disabled_channels );
  if( !$size )
  {
    print_and_log( "parse_hv_db::_process_line - run $run has no dead channels" );
    return;
  }
  
  my $south_size = 0;
  my $north_size = 0;
  
  # create output file
  my $filename = "$output_path/muid.disabledTubes.dat_run$run";
  print_and_log( "creating $filename" );  
  open(OUT, ">$filename");
  foreach my $channel ( sort keys( %disabled_channels ) )
  {
    
    next if( !$disabled_channels{$channel} );
    
    if( $channel =~ /HV_MUID_N_GP\d+/ ) { $north_size++; }
    elsif( $channel =~ /HV_MUID_S_GP\d+/ ) { $south_size++; }
    else {
      print_and_log( "parse_hv_db::_process_line - unrecognized channel: $channel" );
      next;
    }

    print OUT "$channel\n"; 
    
  }
  
  close( OUT );
  
  # print summary
  if( $north_size == 0 ) { print_and_log( "parse_hv_db::_process_line - run $run has no dead channels in north arm" ); }
  elsif( $south_size == 0 ) { print_and_log( "parse_hv_db::_process_line - run $run has no dead channels in south arm" ); }
  else { print_and_log( "parse_hv_db::_process_line - run $run - north: $north_size - south: $south_size" ); }
  
}

#################################################################
sub _get_disabled_channels
{
  
  my $run = $_[0];
  print_and_log( "parse_hv_db::_get_disabled_channels - run: $run" );
  my %out;
  
  ### prepare sql query
  # psql row selection
  my $rows = "name,status,enabled";
    
  # run selection
  my $selection = " runnumber = $run AND subsystem like 'MUID%' AND (status != 1 OR enabled !=  1) ";
  
  # generate psql command
  my $psql_command = "select $rows FROM hvlog_run WHERE $selection";
  my $get_disabled = $daq->prepare($psql_command);
  $get_disabled->execute();
  print_to_log( "parse_hv_db::_get_disabled_channels - psql_command: $psql_command" );    

  my( $channel,$status,$enabled );
  $get_disabled->bind_col(1,  \$channel);
  $get_disabled->bind_col(2,  \$status);
  $get_disabled->bind_col(3,  \$enabled);

  # get output
  while ($get_disabled->fetch) {
    $out{$channel}++;
  }

  $get_disabled->finish();
  
  return %out;
}


#################################################################
sub _get_new_disabled_channels
{
  
  # one should also check the entry time vs begin and end of run time.
  
  # read arguments
  my $run = $_[0];
  my $begin_unix = $_[1];
  my $end_unix = $_[2];
  my %out;
    
  print_and_log( "parse_hv_db::_get_new_disabled_channels - run: $run, begin: $begin_unix, end: $end_unix" );
    
  ### prepare sql query
  # psql row selection
  my $rows = "channelname, st, int4(extract(epoch FROM time)), comment";

  # run selection
  my $selection = " runnumber = $run AND ( channelname like 'HV_MUID_N%' OR channelname like 'HV_MUID_S%') ";
  my $time_selection = "";
  
  # begin time selection
  if( $begin_unix ) { $time_selection .= "extract(epoch FROM time)>=$begin_unix"; }
  
  # end time selection.
  # NOTE: we add 5 minutes to the end_unix time to cover measurements that 
  # occur right at the end of the run.
  if( $end_unix ) {  $time_selection .= " AND extract(epoch FROM time)<=$end_unix+300"; }
  
  my $psql_command = "select $rows FROM hv_triplog WHERE $selection AND $time_selection ORDER BY time";
  my $get_new_disabled = $daq->prepare($psql_command);
  $get_new_disabled->execute();
  print_and_log( "parse_hv_db::_get_new_disabled_channels - psql_command: $psql_command" );    
  my( $channel,$status,$time,$comment );
  $get_new_disabled->bind_col(1,  \$channel);
  $get_new_disabled->bind_col(2,  \$status);
  $get_new_disabled->bind_col(3,  \$time);
  $get_new_disabled->bind_col(4,  \$comment);

  # get output
  while ($get_new_disabled->fetch) {
  
    # check if line corresponds to a trip or a trip clear
    # this is based upon whether or not the "comments" field contains the keyword "cleared"
    my $tripped = !( $comment =~ /cleared/ );

    # parse "channel trip" lines
    if( $tripped )
    {
      if ( $status != 1 )
      {
        
	print_to_log( "parse_hv_db::_get_new_disabled_channels - channel: $channel, status: $status" ); 
	print_and_log( "parse_hv_db::_get_new_disabled_channels - adding channel: $channel - time: $time" );          
	$out{$channel} = $time;
	
      }
        
    } else {
      
      # parse "cleared channel trip" lines
      # if a tripped channel is found that is cleared less than 60 seconds after the trip
      # this is interpreted as a "fake" trip (a communication problem with the HV server), and
      # the channel is removed from the list of disabled channels
      if( $status == 1 && $out{$channel} ) 
      {

	my $trip_time = $out{$channel};
	my $difference = $time - ($trip_time);
	next if( $difference > 60 );
          
	# the channel was found tripping in the same run but recovered. 
	# print_and_log( "parse_hv_db::_get_new_disabled_channels - clearing channel: $channel. time: $time - trip: $trip_time - difference: $difference" );
	$out{$channel} = 0;
          
      }
      
    }
    
  }
    
  $get_new_disabled->finish();
    
  return %out;
}
  
###########################################
# macro usage
sub usage
{
  print_and_log( "usage: parse_hv_db.pl --first-run=XXX [--last-run=XXX] [--output-path=XXX]");
  print_and_log( "");
  print_and_log( "  --runlist=file         run list");                 
  print_and_log( "  --first-run=integer    starting run number");                 
  print_and_log( "  --last-run=integer     ending run number");
  print_and_log( "  --help                 display this help and exit");
  print_and_log( "  --output-path=string   path where to store the files");
  return;
}

###########################################
# print command to screen and log
sub print_and_log 
{
  
  print "$_[0]\n";
  print LOG "$_[0]\n";
  return;
  
}

###########################################
# print command to log
sub print_to_log 
{
 
  print LOG "$_[0]\n";
  return;
  
}
