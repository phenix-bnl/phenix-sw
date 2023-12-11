#!/usr/local/bin/perl

use strict;
use Env;

if (! exists $ENV{"CERN"})
{
#    $CERN = $ENV{"CERN"};
    die "CERN env var not set\n";
}
if (! exists $ENV{"CERN_LEVEL"})
{
    die "CERN_LEVEL env var not set\n";
}
my $CERNLIBS;
my $CERNLIBPATH = $CERN . "/" . $CERN_LEVEL . "/lib";
my $PACKLIB_NOSHIFT = $CERNLIBPATH . "/" . "libpacklib_noshift.a";
my $CERNLIB_COMMAND = ${CERN} . "/" . ${CERN_LEVEL} . "/bin/cernlib";
my $CERNLIB_COMMAND_NOSHIFT = ${CERN} . "/" . ${CERN_LEVEL} . "/bin/cernlib_noshift";
if (! -f $CERNLIB_COMMAND_NOSHIFT)
{
    $CERNLIB_COMMAND_NOSHIFT = $CERNLIB_COMMAND;
}
if (-f $PACKLIB_NOSHIFT)
{
#    $CERNLIBS = `${CERN}/${CERN_LEVEL}/bin/cernlib_noshift geant321 pawlib graflib mathlib kernlib_noshift packlib_noshift mathlib`;
    $CERNLIBS = `$CERNLIB_COMMAND_NOSHIFT geant321 graflib mathlib kernlib_noshift packlib_noshift mathlib`;
}
else
{
#    $CERNLIBS = `${CERN}/${CERN_LEVEL}/bin/cernlib geant321 pawlib graflib mathlib kernlib packlib mathlib`;
    $CERNLIBS = `$CERNLIB_COMMAND geant321 graflib mathlib kernlib packlib mathlib`;
}

$CERNLIBS =~ s%${CERN}/${CERN_LEVEL}/lib/%-l%g;
$CERNLIBS =~ s/-llib/-l/g;
$CERNLIBS =~ s/\.a//g;
$CERNLIBS = "-L" . $CERNLIBPATH . " " . $CERNLIBS;

# now prepending phenix mods, it still needs building so
# it cannot be checked here if it exists (means lets hope the best)

$CERNLIBS = "-L$OFFLINE_MAIN/lib -lphnxgeant " .  $CERNLIBS;

print "$CERNLIBS";
