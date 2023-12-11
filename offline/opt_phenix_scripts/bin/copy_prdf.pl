#!/usr/bin/perl

use strict;
use warnings;
use DBI;
use File::stat;
use Env;

if ($#ARGV < 0)
{
    print "usage: copy_prdf.pl <prdf file> <optional outdir>\n";
    exit(1);
}


my $lfn = $ARGV[0];
my $outdir = "./";
if ($#ARGV == 1)
{
    $outdir = $ARGV[1];
    if (! -d $outdir)
    {
	print "output directory $outdir does not exist\n";
	exit(1);
    }
}

my $dbh = DBI->connect("dbi:ODBC:FileCatalog","argoadmin") || die $DBI::error;
$dbh->{LongReadLen}=2000; # full file paths need to fit in here
my $getfullfile = $dbh->prepare("select full_file_path,fsize from filesstage where lfn = ? and full_host_name = 'hpss' and full_file_path like '/home/phnxsink/%'");

$getfullfile->execute($lfn) || die $DBI::error;
if ($getfullfile->rows > 0)
{
    while(my @res = $getfullfile->fetchrow_array())
    {
	my $dcfile = $res[0];
	my $fsize = $res[1];
	$dcfile =~ s/\/home/\/pnfs\/rcf.bnl.gov\/phenix/;
	print "copying dcfile: $dcfile to $outdir\n";
	if (! -f $dcfile)
	{
	    print "could not locate $dcfile\n";
	    next;
	}
	my $dccpcmd = sprintf("xrdcp -f root://phnxcore03.rcf.bnl.gov:1094%s %s",$dcfile,$outdir);
	system($dccpcmd);
	my $fulloutfile = sprintf("%s/%s",$outdir,$lfn);
	my $disksize = stat($fulloutfile)->size;
	if ($disksize != $fsize)
	{
	    print "size mismatch, size in filecatalog $fsize, size on disk $disksize\n";
	    exit(1);
	}
    }
}
else
{
    print "file $lfn not in catalog - proably not in dCache\n";
    exit(1);
}
$getfullfile->finish();
$dbh->disconnect;

