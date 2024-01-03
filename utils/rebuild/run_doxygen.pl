#!/usr/local/bin/perl

use strict;
use warnings;
use File::Path;
use File::Copy;
use Cwd qw(getcwd realpath);

my $MAXDEPTH       = 5;
my $installsymlink = "/phenix/WWW/offline/doxygen/html";

# clean up old checkout if exists and check out phuniverse
my @cleanup = ( "offline", "simulation" );

foreach my $cdir (@cleanup) {
	if ( -d $cdir ) {
		rmtree($cdir);
	}
}

                                                     
print "#################################\n";                                               
print "cvs co phuniverse_doxygen\n";                                                              
print "#################################\n";        

system("cvs co phuniverse_doxygen");

print "#################################\n";
print "Loading the active taxi modules\n";
print "#################################\n";

my @package = ();
use DBI;
my $dbh = DBI->connect("dbi:ODBC:phnxbld") || die $DBI::error;
my $getpackages = $dbh->prepare(
"select package,contact from anatrainmodules where status > 0 order by ordering"
);
$getpackages->execute() || die $DBI::error;
while ( my @pkts = $getpackages->fetchrow_array() ) {

    if ($pkts[0] eq 'offline/AnalysisTrain/CPCollecivity')
    {
	print "Ignore problematic repository $pkts[0]";
	next;
    }

        if ($pkts[0] eq 'offline/AnalysisTrain/ForwardPhotons_MPCEX16')
	{
	    print "Ignore problematic repository $pkts[0]";
	    next;
	}



#    print "cvs co $pkts[0]\n";
#	system("cvs co $pkts[0]");
}

$getpackages->finish();

print "###################################################################\n";
print "WARNING: please check the local path in Doxyfile is consistent with\n";
print "this directory = " . getcwd() . "\n";
print "###################################################################\n";

# exit;

system("doxygen Doxyfile");
system("cp doxy.log html/");

my $realpath = realpath($installsymlink);

print "realpath: $realpath\n";

( my $linktg, my $number ) = $realpath =~ m/(.*)\.(\d+)$/;

$number++;
if ( $number > $MAXDEPTH ) {
	$number = 1;
}

my $wipearea = sprintf( "%s.%d", $linktg, $number );
if ( -d $wipearea ) {
	rmtree($wipearea);
}
my $movecmd = sprintf( "rsync -al html/ %s/", $wipearea );
print "wipearea: $wipearea\n";
system($movecmd);
unlink $installsymlink;
symlink $wipearea, $installsymlink;
# rmtree("html");
