#!/usr/local/bin/perl
# LuxorLinker script for real data
# Provides all necessary links in a working directory to run all supported
# Luxor analysis chains off of a real data set.  
# This version also cleans up a directory that has been previously set up.
#
# Maintained by Stephen C. Johnson
# Last updated 8/07/01
#
# Last Update 7-11-2003 by Thomas K Hemmick.
#
# Last Update 1-20-2004 by Thomas K Hemmick.  Set run numbers for Run4...
#
#
# If the first argument is a number, then a data set is being selected.
# These values are as follows:

# Usage:
#    Type "LuxorLinker.pl n run", where n is one of the following.
#       n = 0-11 for dataset set-up selection
#       -c to purge the local directory of calibration files
#       -h to print out a help string
#       event is the event number you want to analyze (optional)
#    If you know your run number and not your dataset number,
#       type -1 for dataset and supply the run number.

#    0: Runs 0-4519 (30 GeV, retracted geo, B off)
#    1: Runs 5063-6009 (70 GeV, retracted geo, B off)
#    2: Runs 7688-8712 (70 GeV, standard geo, B on?)
#    3: Runs 8713-8884 (70 GeV, standard geo, B off)
#    4: Runs 8885-10441 (70 GeV, standard geo, B on)
#    5: Runs 10494-10708 (70 GeV, standard geo, B off)
#    6: Runs 10727-11793 (70 GeV, standard geo, B on)
#    7: Runs 11794-11816 (70 GeV, standard geo, B off)
#    8: Runs 11817-19999 (70 GeV, standard geo, B on)
#    9: Runs 20000-41000 (100GeV, standard geo) -- RunII, AuAu and pp
#   10: Runs 41000-99999 (100GeV, standard geo) -- RunIII, dAu and pp
#   11: Runs 100000+     (100GeV, standard geo) -- RunIv, AuAu and pp

# Variable initialization

use strict;
use Env;


my $dataset = $ARGV[0];
my $run = $ARGV[1];

my @dirrun;

$dirrun[0] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/";
$dirrun[1] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/";
$dirrun[2] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/";
$dirrun[3] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/";
$dirrun[4] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/";
$dirrun[5] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/";
$dirrun[6] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/";
$dirrun[7] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/";
$dirrun[8] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/";
$dirrun[9] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/";
$dirrun[10]= "/afs/rhic.bnl.gov/phenix/software/calibration/run2003/";
$dirrun[11]= "/afs/rhic.bnl.gov/phenix/software/calibration/run4/";

my $ndatasets = 28;
my @dirdata;
$dirdata[0] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/dataset0/";
$dirdata[1] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/dataset1/";
$dirdata[2] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/dataset2/";
$dirdata[3] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/dataset3/";
$dirdata[4] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/dataset4/";
$dirdata[5] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/dataset5/";
$dirdata[6] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/dataset6/";
$dirdata[7] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/dataset7/";
$dirdata[8] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/dataset8/";
$dirdata[9] = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/dataset-run2/";
$dirdata[10]= "/afs/rhic.bnl.gov/phenix/software/calibration/run2003/dataset-run3/";
$dirdata[11]= "/afs/rhic.bnl.gov/phenix/software/calibration/run4/dataset-run4/";

# run number ranges for each dataset
my @minrun;
my @maxrun;
$minrun[0] = 0;
$maxrun[0] = 4519;
$minrun[1] = 4520;
$maxrun[1] = 6009;
$minrun[2] = 6010;
$maxrun[2] = 8712;
$minrun[3] = 8713;
$maxrun[3] = 8884;
$minrun[4] = 8885;
$maxrun[4] = 10441;
$minrun[5] = 10442;
$maxrun[5] = 10708;
$minrun[6] = 10709;
$maxrun[6] = 11793;
$minrun[7] = 11794;
$maxrun[7] = 11816;
$minrun[8] = 11817;
$maxrun[8] = 19999;
$minrun[9] = 20000;
$maxrun[9] = 40999;
$minrun[10]= 41000;
$maxrun[10]= 99999;
$minrun[11]=100000;
$maxrun[11]=500000;

# Opening comments to the user
if ($dataset eq "-c") {
  printf("LuxorLinker(Data): Cleaning local directory of ");
  printf("all calibration files.\n");
} elsif ($dataset eq "-x") {
  printf("LuxorLinker(Data): Setting up environment variables.\n");
  printf("   No calibration files will be linked or replaced.\n");
} elsif ($dataset eq "-h") {
  printf("LuxorLinker(Data): Help is on the way...\n");
  printf("   Usage: \n");
  printf("   Type LuxorLinker.pl n <run>, ");
  printf("where n is one of the following.\n");
  printf("     n = 0-27 for dataset set-up selection.\n");
  printf("     n = -1 for run number dataset selection.\n");
  printf("     n = -c to purge the local directory of calibration files\n");
  printf("     n = -x to only set up environment variables.\n");
  printf("     n = -h to print out a help string\n");
  printf("     run = optional run number for specific links\n");
  printf("       If you don't supply a run number, then a default\n");
  printf("       run at the end of the dataset is chosen.\n");
  printf("\n");
  printf("   Datasets:\n");
  printf("     0: Runs 0-4519 (30 GeV, retracted geo, B off)\n");
  printf("     1: Runs 5063-6009 (70 GeV, retracted geo, B off)\n");
  printf("     2: Runs 7688-8712 (70 GeV, standard geo, B on?)\n");
  printf("     3: Runs 8713-8884 (70 GeV, standard geo, B off)\n");
  printf("     4: Runs 8885-10441 (70 GeV, standard geo, B on)\n");
  printf("     5: Runs 10494-10708 (70 GeV, standard geo, B off)\n");
  printf("     6: Runs 10727-11793 (70 GeV, standard geo, B on)\n");
  printf("     7: Runs 11794-11816 (70 GeV, standard geo, B off)\n");
  printf("     8: Runs 11817-19999 (70 GeV, standard geo, B on)\n");
  printf("     9: Runs 20000-40999 (100 GeV, standard geo)--Run2\n");
  printf("    10: Runs 41000-99999 (100 GeV, standard geo)--Run3\n");
  printf("    11: Runs 100000-present (100 GeV, standard geo)--Run4\n");
  printf("    -1: Don't bother me with dataset numbers,");
  printf(" just use my run number.\n");
  printf("\n");
  exit;
} elsif ($dataset >= 0 && $dataset < $ndatasets) {
  printf("LuxorLinker(Data): Setting up local directory for ");
  printf("dataset $dataset analysis.\n");
  if ($run ne "") {
    printf("   Specifying run number $run\n");
    # Check to make sure that the run number and dataset match
    my $icont = 1;
    if ($run < $minrun[$dataset]) {
      $icont = 0;
    }
    if ($run > $maxrun[$dataset]) {
      $icont = 0;
    }
    if ($icont eq 0) {
      printf("Run number is out of the dataset range.  Quitting.\n");
      printf("For dataset $dataset, the run range is ");
      printf("$minrun[$dataset] to $maxrun[$dataset]\n");
      exit;
    }
  }
} elsif ($dataset eq "") {
  printf("LuxorLinker(Data): No command line argument given.\n");
  printf("   Type LuxorLinker.pl -h for a list of arguments.\n");
  exit;
} elsif ($dataset eq "-1") {
  my $i = 0;
  while ($i < $ndatasets) {
    if ($run>=$minrun[$i] && $run<=$maxrun[$i]) {
      $dataset = $i;
    }
    $i++;
  }
  if ($dataset eq "-1") {
    printf("There is no dataset index matching your run number.");
    printf(" Quitting.\n");
    exit;
  }
} else {
  print "$dataset";
  printf("LuxorLinker(Data): Invalid input argument.  Quitting.\n");
  printf("   Type LuxorLinker.pl -h for details.\n");
  exit;
}

# Clean the local directory
if ($dataset eq "-c") {
  # This has to be done for every dataset directory (ugh!)
  opendir(IPATH,$dirrun[$dataset]);
  my @FILES = readdir(IPATH);
  closedir(IPATH);
  foreach my $file (@FILES) {
    # Make sure that the file name is one we want to work with
    # Exclude the directory pointers and all README files.
    my $fcheck = 1;
    if ($file eq "." || $file eq "..") {
      $fcheck = 0;
    }
    my $readme = index($file,"README");
    if ($readme != -1) {
      $fcheck = 0;
    }
    if (-d $file) {
      $fcheck = 0;
    }
    if ($fcheck eq 1) {
      # Check to see if the file exists in the local directory
      if (-e $file) {
	printf("LuxorLinker(Data): Removing $file\n");
	unlink $file;
      }
    }
  }
  my $idir = 0;
  while ($idir < $ndatasets) {
    opendir(IPATH,$dirdata[$idir]);
    my @FILES = readdir(IPATH);
    closedir(IPATH);
    foreach my $file (@FILES) {
      # Make sure that the file name is one we want to work with
      # Exclude the directory pointers and all README files.
      my $fcheck = 1;
      if ($file eq "." || $file eq "..") {
	$fcheck = 0;
      }
      my $readme = index($file,"README");
      if ($readme != -1) {
	$fcheck = 0;
      }
      if (-d $file) {
	$fcheck = 0;
      }
      if ($fcheck eq 1) {
	# Check to see if the file exists in the local directory
	if (-e $file) {
	  printf("LuxorLinker(Data): Removing $file\n");
	  unlink $file;
	}
      }
    }
    $idir++;
  }
}
# dataset = -c

# Create all links to the data for the selected datasets
elsif ($dataset >= 0 && $dataset < $ndatasets) {
  # Do the run2000 directory first
  opendir(IPATH,$dirrun[$dataset]);
  my @FILES = readdir(IPATH);
  closedir(IPATH);
  foreach my $file (@FILES) {
    next if ($file =~ /^emc/);
    # Make sure that the file name is one we want to work with
    # Exclude the directory pointers and all README files.
    my $fcheck = 1;
    if ($file eq "." || $file eq ".." || $file eq "dataset1" || $file eq "dataset2" || $file eq "dataset3" || $file eq "dataset4" || $file eq "dataset5" || $file eq "dataset6" || $file eq "dataset7"|| $file eq "dataset8" || $file eq "dataset0" || $file eq "dataset-run2" || $file eq "root2") {
      $fcheck = 0;
    }
    my $readme = index($file,"README");
    if ($readme != -1) {
      $fcheck = 0;
    }
    if (-d $file) {
      $fcheck = 0;
    }
    if ($fcheck eq 1) {
      # Check to see if the file exists in the local directory
      if (-e $file) {
	printf("LuxorLinker(Data): Replacing $file\n");
	unlink $file;
      }
      # Create the link
      my $linkfile = "$dirrun[$dataset]$file";
      symlink $linkfile, $file;
    }
  }
  # Do the dataset directory last
  opendir(IPATH,$dirdata[$dataset]);
  my @FILES = readdir(IPATH);
  closedir(IPATH);
  foreach my $file (@FILES) {
    next if ($file =~ /^emc/);
    # Make sure that the file name is one we want to work with
    # Exclude the directory pointers and all README files.
    my $fcheck = 1;
    if ($file eq "." || $file eq "..") {
      $fcheck = 0;
    }
    my $readme = index($file,"README");
    if ($readme != -1) {
      $fcheck = 0;
    }
    if (-d $file) {
      $fcheck = 0;
    }
    my $bbccheck = index($file,"bbc5");
    if ($bbccheck != -1) {
      $fcheck = 0;
    }
    if ($fcheck eq 1) {
      # Check to see if the file exists in the local directory
      if (-e $file) {
	printf("LuxorLinker(Data): Replacing $file\n");
	unlink $file;
      }
      # Create the link
      my $linkfile = "$dirdata[$dataset]$file";
      symlink $linkfile, $file;
    }
  }
  # Set up the run-specific links
  # BBC run-specific links
  if ($dataset eq "1") {
    my $nbbcdir = 7;
    my @bbcdir;
    my @bbcmin;
    my @bbcmax;
    $bbcdir[0] = "bbc5213-5337";
    $bbcmin[0] = 0;
    $bbcmax[0] = 5338;
    $bbcdir[1] = "bbc5339-5462";
    $bbcmin[1] = 5339;
    $bbcmax[1] = 5693;
    $bbcdir[2] = "bbc5694-5857";
    $bbcmin[2] = 5694;
    $bbcmax[2] = 5862;
    $bbcdir[3] = "bbc5863-5895";
    $bbcmin[3] = 5863;
    $bbcmax[3] = 5914;
    $bbcdir[4] = "bbc5915-5960";
    $bbcmin[4] = 5915;
    $bbcmax[4] = 5971;
    $bbcdir[5] = "bbc5972-5972";
    $bbcmin[5] = 5972;
    $bbcmax[5] = 5973;
    $bbcdir[6] = "bbc5974-6034";
    $bbcmin[6] = 5974;
    $bbcmax[6] = 6034;
    # Determine which directory to point to based upon
    # run number.  Default is the top directory.
    my $bbcpointer = "";
    my $i = 0;
    while ($i < $nbbcdir) {
      if ($run >= $bbcmin[$i]) {
	if ($run <= $bbcmax[$i]) {
	  $bbcpointer = $bbcdir[$i];
	}
      }
      $i++;
    }
    my $bbcpath = "$dirdata[$dataset]$bbcpointer";
    # Now repeat the link of all files in this directory
    opendir(IPATH,$bbcpath);
    my @FILES = readdir(IPATH);
    closedir(IPATH);
    foreach my $file (@FILES) {
      my $fcheck = 1;
      if ($file eq "." || $file eq "..") {
	$fcheck = 0;
      }
      my $readme = index($file,"README");
      if ($readme != -1) {
	$fcheck = 0;
      }
      if (-d $file) {
	$fcheck = 0;
      }
      if ($fcheck eq 1) {
	if (-e $file) {
	  printf("LuxorLinker(Data): Replacing $file\n");
	  unlink $file;
	}
	# Create the link
	my $linkfile = "$bbcpath/$file";
	symlink $linkfile, $file;
      }
    }
  }
  # BBC dataset = 1
  if ($run eq 12323 || $run eq 12324 || $run eq 12325 || $dataset eq 22 || $dataset eq 27)
    {
      my $oldField = "fieldIntegral.dat";
      my $newField = "fieldIntegral.dat-half";
      my $oldFile = "$dirrun[$dataset]$oldField";
      my $newFile = "$dirrun[$dataset]$newField";
      printf("THIS IS A HALF FIELD RUN.  CHANGING FIELD INTEGRAL FILE\n");
      unlink $oldField;
      symlink $newFile, $oldField;	
    }
  if ($dataset eq 24)
    {
      printf("\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! \n\n");
      printf("THIS IS A REVERSED FIELD RUN.  WE ARE KEEPING THE SAME\n");
      printf("FIELD FILE AS WITH NORMAL POLARITY.  THIS WILL SWITCH\n");
      printf("THE POSITIVE/NEGATIVE DEFINITIONS IN THE MICRO-DST!\n");
      printf("\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! \n");
    }
}
# dataset = number


# Closing comments to the user
if ($dataset eq "-c") {
  printf("\nLuxorLinker(Data): Local directory has been swept clean ");
  printf("of calibrations.\n");
}
if ($dataset eq 0) {
  printf("\nLuxorLinker(Data): Local directory is ready for ");
  printf("dataset 0 analysis.\n");
  printf("  This is valid for runs 0-4519.\n");
  printf("  Run configuration: 30 GeV Au+Au, retracted geo, B off.\n");
  if ($run ne "") {
    printf("  You have specified run $run\n");
  }
}
if ($dataset eq 1) {
  printf("\nLuxorLinker(Data): Local directory is ready for ");
  printf("dataset 1 analysis.\n");
  printf("  This is valid for runs 5063-6009.\n");
  printf("  Run configuration: 70 GeV Au+Au, retracted geo, B off.\n");
  if ($run ne "") {
    printf("  You have specified run $run\n");
  }
}
if ($dataset eq 2) {
  printf("\nLuxorLinker(Data): Local directory is ready for ");
  printf("dataset 2 analysis.\n");
  printf("  This is valid for runs 7688-8712.\n");
  printf("  Run configuration: 70 GeV Au+Au, standard geo, B on.\n");
  if ($run ne "") {
    printf("  You have specified run $run\n");
  }
}
if ($dataset eq 3) {
  printf("\nLuxorLinker(Data): Local directory is ready for ");
  printf("dataset 3 analysis.\n");
  printf("  This is valid for runs 8713-8884.\n");
  printf("  Run configuration: 70 GeV Au+Au, standard geo, B off.\n");
  if ($run ne "") {
    printf("  You have specified run $run\n");
  }
}
if ($dataset eq 4) {
  printf("\nLuxorLinker(Data): Local directory is ready for ");
  printf("dataset 4 analysis.\n");
  printf("  This is valid for runs 8885-10441.\n");
  printf("  Run configuration: 70 GeV Au+Au, standard geo, B on.\n");
  if ($run ne "") {
    printf("  You have specified run $run\n");
  }
}
if ($dataset eq 5) {
  printf("\nLuxorLinker(Data): Local directory is ready for ");
  printf("dataset 5 analysis.\n");
  printf("  This is valid for runs 10494-10708.\n");
  printf("  Run configuration: 70 GeV Au+Au, standard geo, B off.\n");
  if ($run ne "") {
    printf("  You have specified run $run\n");
  }
}
if ($dataset eq 6) {
  printf("\nLuxorLinker(Data): Local directory is ready for ");
  printf("dataset 6 analysis.\n");
  printf("  This is valid for runs 10727-11793.\n");
  printf("  Run configuration: 70 GeV Au+Au, standard geo, B on.\n");
  if ($run ne "") {
    printf("  You have specified run $run\n");
  }
}
if ($dataset eq 7) {
  printf("\nLuxorLinker(Data): Local directory is ready for ");
  printf("dataset 7 analysis.\n");
  printf("  This is valid for runs 11794-11816.\n");
  printf("  Run configuration: 70 GeV Au+Au, standard geo, B off.\n");
  if ($run ne "") {
    printf("  You have specified run $run\n");
  }
}
if ($dataset eq 8) {
  printf("\nLuxorLinker(Data): Local directory is ready for ");
  printf("dataset 8 analysis.\n");
  printf("  This is valid for runs 11817-19999.\n");
  printf("  Run configuration: 70 GeV Au+Au, standard geo, B on.\n");
  if ($run ne "") {
    printf("  You have specified run $run\n");
  }
}
if ($dataset eq 9) {
  printf("\nLuxorLinker(Data): Local directory is ready for ");
  printf(" run II reconstruction.\n");
  printf("  Run configuration: 100 GeV Au+Au, standard geo\n");
  if ($run ne "") {
    printf("  You have specified run $run\n");
  }
}
if ($dataset eq 10) {
  printf("\nLuxorLinker(Data): Local directory is ready for ");
  printf(" run III reconstruction.\n");
  printf("  Run configuration: 100 GeV d+Au and p+p\n");
  if ($run ne "") {
    printf("  You have specified run $run\n");
  }
}
if ($dataset eq 11) {
    printf("\nLuxorLinker(Data): Local directory is ready for ");
    printf(" run IV and beyond reconstruction.\n");
    if ($run ne "") {
	printf("  You have specified run $run\n");
    }
    if ($run > 220000 && $run < 241000) # Run 7
    {
	if ($run > 236105 || $run < 235880) # run 235880-236105 was ++ field
	{
	    my $oldField = "fieldIntegral.dat";
	    my $newField = "fieldIntegral+-.dat.run07";
	    my $oldFile = "$dirrun[$dataset]$oldField";
	    my $newFile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2007/$newField";
	    printf("THIS IS A +- FIELD RUN.  CHANGING FIELD INTEGRAL FILE\n");
	    unlink $oldField;
	    symlink $newFile, $oldField;	
	}
    }
    elsif ($run > 272000 && $run < 275723) # Run 9 first part +- field
    {
	my $oldField = "fieldIntegral.dat";
	my $newField = "fieldIntegral+-.dat.run07";
	my $oldFile = "$dirrun[$dataset]$oldField";
	my $newFile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2007/$newField";
	printf("THIS IS A +- FIELD RUN.  CHANGING FIELD INTEGRAL FILE\n");
	unlink $oldField;
	symlink $newFile, $oldField;	
    }
    elsif ($run > 280828 && $run < 318000) # Run 9,10 200GeV +- field
    {
	my $oldField = "fieldIntegral.dat";
	my $newField = "fieldIntegral+-.dat.run07";
	my $oldFile = "$dirrun[$dataset]$oldField";
	my $newFile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2007/$newField";
	printf("THIS IS A +- FIELD RUN.  CHANGING FIELD INTEGRAL FILE\n");
	unlink $oldField;
	symlink $newFile, $oldField;	
    }
# muon material map
    my $pisafile = "pisafile.dat.cZ";
    if ($run > 220000 && $run < 260000) # Run 7,8
    {
	print "Using Muon arm Material map from Run 7/8\n";
	my $newpisafile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2007/" . $pisafile;
	unlink $pisafile;
        symlink $newpisafile, $pisafile;
    }
    elsif ($run > 272000 && $run < 292000) # Run 9
    {
	print "Using Muon arm Material map from Run 9\n";
	my $newpisafile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2009/" . $pisafile;
	unlink $pisafile;
        symlink $newpisafile, $pisafile;
    }
    elsif ($run > 300000 && $run < 320000) # Run 10
    {
	print "Using Muon arm Material map from Run 10\n";
	my $newpisafile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2010/" . $pisafile;
	unlink $pisafile;
        symlink $newpisafile, $pisafile;
    }
    elsif ($run >= 320000&& $run < 351000 ) # Run 11++
    {
	print "Using Muon arm Material map from Run 11\n";
	my $newpisafile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2011/" . $pisafile;
	unlink $pisafile;
        symlink $newpisafile, $pisafile;
    }

    elsif ($run >= 351000 ) # Run 12++
    {
	print "Using Muon arm Material map from Run 12\n";
	my $newpisafile = "/afs/rhic.bnl.gov/phenix/software/calibration/run12/" . $pisafile;
	unlink $pisafile;
        symlink $newpisafile, $pisafile;
    }

}
if (-f "md5sum.list")
{
    unlink "md5sum.list";
}
printf("\n");
if ($dataset >= 0 && $dataset < $ndatasets) {
  printf("LuxorLinker(Data): Make sure that you have the proper analysis\n");
  printf("   macros and are pointing to a data PRDF file before proceeding\n");
  printf("\n");
}
