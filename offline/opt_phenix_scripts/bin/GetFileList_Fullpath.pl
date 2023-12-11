#!/usr/local/bin/perl

use DBI;

$dbh = DBI->connect("dbi:ODBC:FileCatalog","argoadmin") || die $DBI::error;
$dbh->{LongReadLen}=2000; # full file paths need to fit in here

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
$askFrog = $dbh->prepare("SELECT full_file_path,full_host_name FROM files WHERE lfn= ? and full_host_name <> 'hpss' limit 1");

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
	$askFrog->execute($file[0]);
	if ($askFrog->rows > 0)
	{
	    @dfile = $askFrog->fetchrow_array();

	    if ($#ARGV == 2)
	    {
		print F "$dfile[0]\n";
	    }
	    else
	    {
		print "$dfile[0]\n";
	    }
	}
    }
}
else
{
    print STDERR "No files for dataset $dataset, filetype $ARGV[1]\n";
}

$getFiles->finish();
$askFrog->finish();
$dbh->disconnect;
if ($#ARGV == 2)
{
    close(F);
}
