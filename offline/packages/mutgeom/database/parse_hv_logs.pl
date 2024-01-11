#! /usr/bin/perl

###############################################################
# $Id: parse_hv_logs.pl,v 1.15 2009/08/24 15:01:22 shoji Exp $
# make list of runs from posgres run database and retrieve corresponding timestamps

use strict;
use Getopt::Long;
use Env;
use POSIX;
use Time::localtime;
use DBI;

##################################
# Main

## prepare logfile
my $logfile = "parse_hv_logs.log";
print( "parse_hv_logs - writting log to $logfile\n" );
open(LOG, ">$logfile");

## prepare the daq DB
my $daq = DBI->connect("dbi:ODBC:daq") || die $DBI::error;

## prepare summary
my $summaryfile = "parse_hv_logs.summary";
print( "parse_hv_logs - writting summary to $summaryfile\n" );
open(SUMMARY, ">$summaryfile");

# retrieve options
print_and_log( "parse_hv_logs.pl" );
  
my ($opt_runlist,$opt_first_run,$opt_last_run,$opt_output_path,$opt_help,$opt_suppress);
GetOptions(
	   'runlist:s' => \$opt_runlist,
	   'first-run:s' => \$opt_first_run,
	   'last-run:s' => \$opt_last_run,
	   'output-path:s' => \$opt_output_path,
	   'help' => \$opt_help,
	   'suppress' => \$opt_suppress
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
  print( "parse_hv_logs - input_runs: @runs" );

  # first run
  $first_run = $runs[0];
  $last_run = $runs[-1];

  # copy list of runs into hash table
  @selected_runs{@runs}=();

} else {

  # retrieve first and last run
  if( !$opt_first_run )
  {
    print_and_log( "parse_hv_logs - first run not specified" );
    usage();
    exit;
  }

  $first_run = $opt_first_run;
  $last_run = $opt_last_run;

}

print_and_log( "parse_hv_logs - first run: $first_run" );
print_and_log( "parse_hv_logs - last run: $last_run" );

## load conversion map 
## it is used to convert Bulk name into 
my $channel_map_file = "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/simulation/mutr_HVtoDisabledCh_map.txt";
my %channel_map = _parse_channel_map( $channel_map_file );
    
my $output_path = ".";
if( $opt_output_path ) { $output_path = $opt_output_path; }
print_and_log( "parse_hv_logs - output path: $output_path" );


# psql row selection
my $rows = "Run.RunNumber,Run.BRUnixTime,Run.ERUnixTime,cast(Run.BRTimeStamp as Date),cast(Run.ERTimeStamp as Date),Run.EventsInRun";

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
print_and_log( "parse_hv_logs::_select_runs - psql_command: $psql_command" );
print_and_log( "" );

my $size = $get_runs->rows;
print_and_log( "parse_hv_logs - found $size lines" );

my( $run,$begin_unix,$end_unix,$begin_day,$end_day,$events );
$get_runs->bind_col(1, \$run);
$get_runs->bind_col(2, \$begin_unix);
$get_runs->bind_col(3, \$end_unix);
$get_runs->bind_col(4, \$begin_day);
$get_runs->bind_col(5, \$end_day);
$get_runs->bind_col(6, \$events);

while ($get_runs->fetch) {
  if( $opt_runlist && !(exists $selected_runs{$run}) ) { print_and_log( "run $run discarded." ); }
  else { _process_line( $run,$begin_unix,$end_unix,$begin_day,$end_day,$events,$opt_suppress ); }
}
$get_runs->finish();

print_and_log( "parse_hv_logs - completed." );

close( SUMMARY );
close( LOG );
$daq->disconnect;

exit; 

###########################################
# process each output line from psql. Get run number using regular expression
sub _get_run
{
  if( !( $_[0] =~ /\s*(\d+)\s*\|/ ) )
  { 
    print_and_log( "parse_hv_logs::_get_run - unable to get run number from line $_[0]" );
    return 0;
  }
  
  return $1;
  
}

###########################################
# process each output line from psql
sub _process_line
{
  
  my $run = $_[0];
  my $begin_unix = $_[1];
  my $end_unix=$_[2];
  my $begin_day=$_[3];
  my $end_day=$_[4];
  my $events=$_[5];
  my $opt_suppress=$_[6];

  # keep track of first measurement time
  my $first_measurement_time = 0;
  
  # check parsing
  if( $begin_unix == "" ) 
  { 
    print_and_log( "parse_hv_logs::_process_line - run $run has invalid begin time" ); 
    return; 
  }
  
  if( $end_unix == "" ) {
    print_and_log( "parse_hv_logs::_process_line - run $run has invalid end time" ); 
    return;
  }
     
  print_and_log( "\nparse_hv_logs::_process_line - run $run - begin: $begin_unix end: $end_unix" );
  print_and_log( "parse_hv_logs::_process_line - run $run - begin: $begin_day end: $end_day" );
  
  # retrieve north arm filenames matching begin and end timestamp
  # keep track of how many channel trip during the run.
  my $new_dead_channels = 0;
  my @north_files = _get_files( "North", $begin_day, $end_day );
  my %dead_channels_north = _process_files( @north_files, $begin_unix, $end_unix, $new_dead_channels );
  my $new_channels_count_north = $new_dead_channels;
  
  printf SUMMARY ("%10i %10i %10i %s %s\n", $run, $begin_unix, $end_unix, @north_files );

  # print dead channels and relevant time information
  my $north_size = 0;
  foreach my $channel( sort keys( %dead_channels_north ) )
  { 
    
    # skip channel if associated trip time is zero
    # this usually corresponds to channel that are "recovered" during runs, which, in turn is characteristic
    # for a fake trip
    next if( !$dead_channels_north{$channel} );
    
    print_and_log( "parse_hv_logs::trip "
      ." $run "
      ._convert_name($channel)
      ." $dead_channels_north{$channel}"
      ." $begin_unix $end_unix $first_measurement_time" ); 
    $north_size++;
    
  }
  
  # retrieve south arm filenames matching begin and end timestamp
  # keep track of how many channel trip during the run.
  my $new_dead_channels = 0;
  my @south_files = _get_files( "South", $begin_day, $end_day );
  my %dead_channels_south = _process_files( @south_files, $begin_unix, $end_unix, $new_dead_channels );
  my $new_channels_count_south = $new_dead_channels;

  printf SUMMARY ("%10i %10i %10i %s %s\n", $run, $begin_unix, $end_unix, @south_files );

  # print dead channels and relevant time information
  my $south_size = 0;
  foreach my $channel( sort keys( %dead_channels_south ) )
  { 
    
    # skip channel if associated trip time is zero
    # this usually corresponds to channel that are "recovered" during runs, which, in turn is characteristic
    # for a fake trip
    next if( !$dead_channels_south{$channel} );
    
    print_and_log( "parse_hv_logs::trip "
      ." $run "
      ._convert_name($channel)
      ." $dead_channels_south{$channel}"
      ." $begin_unix $end_unix $first_measurement_time" ); 
    $south_size++;
    
  }
  
  
  # check sizes and print error messages accordingly
  if( $north_size == 0 && $south_size == 0 )
  {
    print_and_log( "parse_hv_logs::_process_line - run $run has no dead channels" );
    return;
  }
  
  print_and_log( "parse_hv_logs::_process_line - run $run new channels south: $new_channels_count_south north: $new_channels_count_north" );

  if( $north_size == 0 ) { print_and_log( "parse_hv_logs::_process_line - run $run has no dead channels in north arm" ); }
  elsif( $south_size == 0 ) { print_and_log( "parse_hv_logs::_process_line - run $run has no dead channels in south arm" ); }
  else { print_and_log( "parse_hv_logs::_process_line - run $run - north: $north_size - south: $south_size" ); }
  
  if ( !$opt_suppress ) {
  # create output file
  my $filename = "$output_path/mut.disabledAnodes.dat_run$run";
  print_and_log( "creating $filename" );
  
  open(OUT, ">$filename");
  foreach my $channel( sort keys (%dead_channels_north) )
  { 
    # skip channel if it re-appeared during the run
    next if( !$dead_channels_north{$channel} );
    
    # convert bulk name into geometrical name
    $channel = _convert_name( $channel );
    print OUT "$channel\n"; 
  }
  
  foreach my $channel( sort keys (%dead_channels_south) )
  { 

    # skip channel if it re-appeared during the run
    next if( !$dead_channels_south{$channel} );

    # convert bulk name into geometrical name
    $channel = _convert_name( $channel );
    print OUT "$channel\n"; 
  }
  
  close( OUT );
}

}

###########################################
# previous day
sub _get_previous_day
{
  my $current_day = $_[0];

  # parse string and 
  if( !( $current_day =~ /(\d+)\-(\d+)\-(\d+)/ ) ) { return $current_day; }
  my $year = $1;
  my $month = $2-1;
  my $day = $3;
  
  # convert to unix time, remove one day and convert back
  my $unix_time = mktime (0, 0, 0, $day, $month, $year-1900, 0, 0);  
  $unix_time -= (24*3600);
  
  $year = localtime($unix_time)->year()+1900;
  $month = localtime($unix_time)->mon()+1;
  $day = localtime($unix_time)->mday();
  
  if( $month<10) { $month = "0$month"; }
  if( $day<10 ) { $day="0$day"; }
  
  return "$year-$month-$day";
  
}

###########################################
# get files matching begin and end time stamps
sub _get_files
{
  my $path = "/home/phoncs/mutr/hv/runlog";
  
  my $arm = $_[0];
  my $begin_ts = $_[1];
  my $end_ts = $_[2];
  
  
  my @files =();
  if( $begin_ts == $end_ts ) 
  { 
    
    # retrieve previous day
    # this is due to the fact that logfiles are stored
    # at some point during current day with a stamp dated from the day before.
    # a given run must then be stored either in the file dated by current day, 
    # or by the day before.
    my $prev_ts = _get_previous_day( $begin_ts );
  
    push( @files, "$path/MutrHV_$arm$prev_ts.log" ); 
    push( @files, "$path/MutrHV_$arm$begin_ts.log" ); 

  } else {     
    
    push( @files, "$path/MutrHV_$arm$begin_ts.log" ); 
    push( @files, "$path/MutrHV_$arm$end_ts.log" ); 
    
  }
  
  return @files;
  
}

###########################################
# process a file. Find lines matching begin and end timestamp
sub _process_files
{

  # store files
  my $first_file = $_[0];
  my $second_file = $_[1];

  # store timestamp
  my $begin_unix = $_[2];
  my $end_unix = $_[3];

  my $new_dead_channels = $_[4];

  # list of files to be processed
  my @files = ();
  
  # to spare time, check if first line of second file is before the begin stamp
  open( IN, $second_file );
  while( <IN> )
  {
      
    # get timestamp using regular expression
    my $line=$_;
    next if( !($line =~ /(\d+) .+/) );
    my $time = $1;
    
    if( $time > $begin_unix )
    {
      push( @files, $first_file );
      push( @files, $second_file );
    } else { push( @files, $second_file ); }
    
    # break
    last if (1);
    
  };
  close( IN );

  # for debugging, make sure that the time stamp of at least one of the last ten lines of the second file is higher than run number end of time
  if( -f $second_file )
  {
    my $valid = 0;
    my @lines = split( /\n/, `tail -10 $second_file` );
    foreach my $line( @lines )
    {
      next if( !($line =~ /(\d+) .+/) );
      my $time = $1;
      if( $time >= $end_unix )
      { 
        $valid = 1;
        last;
      }
    }
    
    if( !$valid )
    {
      print_and_log( "parse_hv_logs::_process_files - something wrong run time-range does not match logfiles." );
      exit(0);
    }
  } else {
    print_and_log( "parse_hv_logs::_process_files - file $second_file not found." );
  }
  
  # dead_channels is used to store dead channels
  # seen is used to ensure unicity
  my %bad_channels;

  # also keep track of good channels to look for channels that trip during run
  my %good_channels;
  
  # keep first measurement time
  my $first_measurement_time = 0;
  
  # process files
  foreach my $file( @files )
  {
    my $time = "";
    
    open(IN, "$file");
    print_and_log( "parse_hv_logs::_process_files - processing $file" );  
    while (<IN>)
    {
      
      # line format:
      # 1074411711 Sun Jan 18 02:41:51 EST 2004 S1220 3 0 1849
      # first number is the unix time stamp
      # second number is the plain time stamp
      # then comes the channel number
      # then comes the status
      # then comes the applied HV (is it the command or the actual value. To be checked in the log file)
      
      # get timestamp using regular expression
      my $line=$_;
      chomp( $line );
      
      # get timestamp using regular expression
      $line =~ /(\d+).+/;
      $time = $1;
      
      # check if time match
      next if ( $time < $begin_unix );
      last if ( $time > $end_unix );

      # set first measurement
      if( (!$first_measurement_time) || $time < $first_measurement_time ) 
      { 
        $first_measurement_time = $time;
        print_and_log( "parse_hv_logs::_process_files - first_measurement_time: $first_measurement_time" );
      }

      # retrieve channel and status
      if( !( $line =~ /\d+\s+\w+\s+\w+\s+\d+\s+\S+\s+\w+\s+\d+\s+(\S+)\s+(\d+)\s+(\d+)\s+(\d+(\.\d+)?)/ ) )
      { print_and_log( "parse_hv_logs::_process_files - skipping line $line" ); }
      
      my $channel = $1;
      my $bulk_status = $2;
      my $chan_status = $3;
      my $hv = $4;
      
      # check channel status
      if( !( $bulk_status == 1 && $chan_status == 1 && $hv > 1700 ) )
      {
        
        # print_and_log( $line );
        if( !$bad_channels{$channel} ) { $bad_channels{$channel} = $time; }
        
        # see if channel was good before
        if( $good_channels{$channel} )
        { 
          print_and_log( "Warning: $channel was ON and is now OFF - time: $time" );
          $good_channels{$channel} = 0;
          $new_dead_channels++;
        }
        
      } else {
        
        # store in good channels list
        $good_channels{$channel} = 1;
        if( $bad_channels{$channel} ) 
        { 
          print_and_log( "Warning: $channel was OFF and is now ON - time: $time." ); 
          $bad_channels{$channel} = 0;
          $new_dead_channels--;
        }
        
      }
      
    }
    
    close(IN);
    
    last if ( $time > $end_unix );
  
  }
  
  # sort array and return
  return %bad_channels;
  
}

#################################################################
# reads conversion between bulk name and geometrical channel name
sub _parse_channel_map
{
  my $file = $_[0];
  print( "parse_hv_logs::_parse_channel_map - reading file $file\n" );
  
  my %channel_map;
  my @rows = split( /\n/, `more $file` );
  foreach my $row( @rows )
  {
    
    # try parse
    if( !( $row =~ /^\s*(\d+)\s+(\d+)\s*$/ ) )
    {
      print( "parse_hv_logs::_parse_channel_map - skipping line $row\n" );
      next; 
    }
    
    # store in map
    $channel_map{$1} = $2;
    
  }
  
  return %channel_map;
}


#########################################
# convert bulk name into geometrical name
sub _convert_name
{
  my $name = $_[0];
  
  #try parse row
  if( !( $name =~ /(N|S)(\d+)/ ) )
  { 
    print( "parse_hv_logs::_convert_name - skipping $name\n" );
    next;
  }
  
  my $arm = $1;
  my $channel = $2;
  my $new_channel = $channel_map{$channel};
  if( !$new_channel ) { $new_channel = $channel; }  

  my $new_name = "$arm$new_channel";
  
  return $new_name;
  
}

###########################################
# macro usage
sub usage
{
  print_and_log( "usage: parse_hv_logs.pl --first-run=XXX [--last-run=XXX] [--output-path=XXX] [--suppress]" );
  print_and_log( "\n");
  print_and_log( "  --runlist=string      list of runs to be processed" );
  print_and_log( "  --first-run=integer    starting run number");                 
  print_and_log( "  --last-run=integer     ending run number");
  print_and_log( "  --help                 display this help and exit");
  print_and_log( "  --output-path=string   path where to store the files");
  print_and_log( "  --suppress             add if not dump dat file" );
  
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
