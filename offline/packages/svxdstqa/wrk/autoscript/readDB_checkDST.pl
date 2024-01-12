#! /usr/bin/perl -w

use strict;
use DBI;
use File::Basename;

our $prodName;

require "/home/phnxreco/run11/AutoVtx/launch/config.pl";

if ($#ARGV < 0)
{
    print "usage: recoDB_checkDST.pl <run>\n";
    exit(-2);
}

my $run = $ARGV[0];

my $reco = DBI->connect("dbi:ODBC:phnxreco_1008") || die $DBI::error;

my $GetStatus = $reco->prepare("select status from prodjob_$prodName where runnumber=$run");
$GetStatus->execute() || die $DBI::error;
my $status = -100;
if ($GetStatus->rows > 0)
{
    my @row = $GetStatus->fetchrow_array;
  if (defined $row[0])
  {
    $status = $row[0];
  }
}

$GetStatus->finish();

$reco->disconnect; 

print "$status\n";

