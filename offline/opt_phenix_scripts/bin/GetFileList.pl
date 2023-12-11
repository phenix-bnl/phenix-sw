#!/usr/local/bin/perl

use DBI;

$dbh = DBI->connect("dbi:ODBC:FileCatalog","","", {PrintError=>0}) || die $DBI::error;
$dbh->{LongReadLen}=2000; # increase char arrays from default 80 to 2000

push(@filetype,"CNT");
push(@filetype,"CWG");
push(@filetype,"DST_ALL");
push(@filetype,"DST_DCHHit");
push(@filetype,"DST_DCHTrk");
push(@filetype,"DST_EMCTwr");
push(@filetype,"DST_EVE");
push(@filetype,"DST_MPC");
push(@filetype,"DST_PAD");
push(@filetype,"DST_RXNP");
push(@filetype,"DST_TEC");
push(@filetype,"EWG");
push(@filetype,"Hard");
push(@filetype,"LVL2");
push(@filetype,"MWG");
push(@filetype,"PWG");

if ( $#ARGV + 1 < 2 ||  $#ARGV + 1 > 3 ) 
{
    print "usage: GetFileList.pl dataset dsttype <optional listfile>\n" ;
    $getdataset = $dbh->prepare("select distinct dataset from diskfiles order by dataset");
    $getdataset->execute();
    print "\n\npossible datasets are: \n";
    while(@ds = $getdataset->fetchrow_array())
    {
	print "$ds[0]\n";
    }
    print "\npossible filetypes are: \n";
    for(my $n=0; $n<=$#filetype;$n++)
    {
	print "$filetype[$n]\n";
    }
    print "\nnot all filetypes exist for all datasets\n";
    $getdataset->finish();
    $dbh->disconnect;
    exit (-2);
}

$dataset = $ARGV[0];
$datatype = $ARGV[1] . "%";

$getFiles = $dbh->prepare("select filename from diskfiles where dataset = ? and filename like '$datatype' and status > 0 order by runnumber,segment");

$getFiles->execute($dataset);

if ($#ARGV == 2)
{
    $listfile = $ARGV[2];
    print "Writing output for dataset $dataset, filetype $ARGV[1] to $listfile\n";
    open(F,">$listfile");
} 


if ($getFiles->rows > 0)
{
    while(@file = $getFiles->fetchrow_array())
    {
	if ($#ARGV == 2)
	{
	    print F "$file[0]\n";
	}
	else
	{
	    print "$file[0]\n";
	}
    }
}
else
{
    print STDERR "No files for dataset $dataset, filetype $ARGV[1]\n";
}

$getFiles->finish();
$dbh->disconnect;
if ($#ARGV == 2)
{
    close(F);
}
