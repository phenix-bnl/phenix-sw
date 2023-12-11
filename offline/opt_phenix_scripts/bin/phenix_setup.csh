#! /bin/csh -f

# A general purpose login script for PHENIX.  The allowed arguments
# are '-a' and '-n' 
# -a indicates that the script should append to the PATH
# and LD_LIBRARY_PATH rather than replace them, and a trailing
# argument used to indicate the version of the installed software to
# use.  
# -n forces the unset of all relevant variables so you can switch between
# 32 bit and 64 bit setups
# For instance, "new" (also the default value) will point you to
# software in /afs/rhic.bnl.gov/phenix/software/new.  You can be specific if
# you need to be.  Specifying "pro.5" will point you to software in
# /afs/rhic.bnl.gov/phenix/software/pro.5

# Usage: source phenix_setup.csh [-a] [-n] [version]

# use "limit coredumpsize unlimited" to undo this.
limit coredumpsize 0

set opt_a = 0
set opt_n = 0
set opt_v = "new"
foreach arg ($*)
    switch ($arg)
    case "-a":
	set opt_a = 1
	breaksw
    case "-n":
        set opt_n = 1
	breaksw
    case "-*":
        echo "usage source phenix_setup.csh [-a] [-n] [-h] [version]"
        echo "-a: append path and LD_LIBRARY_PATH to existing ones"
        echo "-n: overwrite all environment variables, needed for switching 32 <-> 64 bit"
        echo "version: build version (new, ana, pro, play,... - also with version number e.g. ana.407)"
        exit(0)
	breaksw
     case "*":
        set opt_v = $arg
	breaksw
    endsw
end

# if -n unset all relevant environment variables
if ($opt_n) then
  unsetenv CALIBRATIONROOT
  unsetenv CC
  unsetenv CERN*
  unsetenv COMPILER_PATH
  unsetenv CONFIG_SITE
  unsetenv COVERITY_ROOT
  unsetenv CXX
  unsetenv FC
  unsetenv G4*
  unsetenv GSEARCHPATH
  unsetenv LHAPATH
  unsetenv MANPATH
  unsetenv OFFLINE_MAIN
  unsetenv ONLINE_MAIN
  unsetenv OPT_*
  unsetenv PARASOFT
  unsetenv PERL5LIB
  unsetenv PGHOST
  unsetenv PYTHIA8
  unsetenv PYTHONPATH
  unsetenv ROOT_INCLUDE_PATH
  unsetenv ROOTSYS
  unsetenv SIMULATION_MAIN
  unsetenv TSEARCHPATH
  unsetenv XERCESCROOT
endif

set sysname=`/usr/bin/fs sysname | sed "s/^.*'\(.*\)'.*/\1/"`

# set LANG env var so compiler errors come out correctly
# instead of a`
setenv LANG C

# turn off gtk warning about accessibility bus
setenv NO_AT_BRIDGE 1

# speed up DCache
setenv DCACHE_RAHEAD
setenv DCACHE_RA_BUFFER 2097152

# Make copies of PATH and LD_LIBRARY_PATH as they were
setenv ORIG_PATH ${PATH}
if ($?LD_LIBRARY_PATH) then
    setenv ORIG_LD_LIBRARY_PATH ${LD_LIBRARY_PATH}
else
    unsetenv ORIG_LD_LIBRARY_PATH
endif

if ($?MANPATH) then
    setenv ORIG_MANPATH ${MANPATH}
else
    unsetenv ORIG_MANPATH
endif

if (! $?OPT_PHENIX) then
  if (-d /opt/phenix/core ) then
    setenv OPT_PHENIX /opt/phenix/core
  else
    if (-d /opt/phenix ) then
      setenv OPT_PHENIX /opt/phenix
    endif
  endif
endif

if (! $?OPT_PHENIX_UTILS) then
  if (-d /opt/phenix/utils) then
    setenv OPT_PHENIX_UTILS /opt/phenix/utils
  else
    if (-d /opt/phenix ) then
      setenv OPT_PHENIX_UTILS /opt/phenix
    endif
  endif
endif

# set site wide compiler options (e.g. -m32 and  no rpath hardcoding)
setenv CONFIG_SITE ${OPT_PHENIX}/etc/config.site


# Perl
if (! $?PERL5LIB) then
   if (-d ${OPT_PHENIX_UTILS}/lib64/perl5) then
#    setenv LD_PRELOAD /opt/phenix/64/lib/libodbc.so
     setenv PERL5LIB ${OPT_PHENIX_UTILS}/lib64/perl5:${OPT_PHENIX_UTILS}/share/perl5
   endif
endif

# CERN Libraries
if (! $?CERN) then
    setenv CERN /cern
endif 
if (! $?CERN_LEVEL) then
    setenv CERN_LEVEL pro
endif 

# OFFLINE
if (! $?OFFLINE_MAIN) then
  if (! -d /afs/rhic.bnl.gov/phenix/PHENIX_LIB/sys/$sysname/new/../$opt_v) then
    set opt_v = "new"
  endif
  setenv OFFLINE_MAIN /afs/rhic.bnl.gov/phenix/PHENIX_LIB/sys/$sysname/new/../$opt_v
endif

if ($OFFLINE_MAIN =~ *"insure"* ) then
  setenv G_SLICE always-malloc
else
  if ($?G_SLICE) then
    unsetenv G_SLICE
  endif
endif

# Normalize OFFLINE_MAIN 
if (-d $OFFLINE_MAIN) then
  set here=`pwd`
  cd $OFFLINE_MAIN
  set there=`pwd`
  setenv OFFLINE_MAIN `echo $there | sed "s/@sys/$sysname/g"`
  cd $here
endif

# set Pythia8 to copy in OFFLINE_MAIN if not set
if (! $?PYTHIA8) then
  if (-d $OFFLINE_MAIN/share/Pythia8) then
    setenv PYTHIA8 $OFFLINE_MAIN/share/Pythia8
  endif
endif

# ROOT
if (! $?ROOTSYS) then
    if (-d $OFFLINE_MAIN/root) then
      setenv ROOTSYS $OFFLINE_MAIN/root
    else    
      setenv ROOTSYS $OPT_PHENIX/root
    endif    
endif

# taxi data file search path
if (! $?TSEARCHPATH) then
  if ($?OFFLINE_MAIN) then
    setenv TSEARCHPATH .:${OFFLINE_MAIN}:/afs/rhic.bnl.gov/phenix/PHENIX_LIB/sys/$sysname/taxi_large
  else
    setenv TSEARCHPATH ./
  endif
endif

if (! $?PGHOST) then
  setenv PGHOST phnxdbrcf2
  setenv PGUSER phnxrc
  setenv PG_PHENIX_DBNAME Phenix_phnxdbrcf2_C
endif

# Simulation
if (! $?SIMULATION_MAIN) then
    setenv SIMULATION_MAIN /afs/rhic.bnl.gov/phenix/software/simulation/pro/$sysname
endif

# CVS repository
if (! $?CVSROOT) then
    setenv CVSROOT /afs/rhic.bnl.gov/phenix/PHENIX_CVS
endif

# Basic PATH
switch ($HOSTTYPE) 
  case *linux*:
    set path = (/usr/lib64/qt-3.3/bin /usr/local/bin /bin /usr/bin /usr/local/sbin /usr/sbin)
    set manpath = `/usr/bin/man --path`
    breaksw

endsw

if (-d $OPT_PHENIX/bin) then
  set path = ($OPT_PHENIX/bin $path)
endif
if (-d $OPT_PHENIX_UTILS/bin) then
  set path = ($OPT_PHENIX_UTILS/bin $path)
endif

set path = (. $path)

if (-d ${OPT_PHENIX}/man) then
    set manpath = ${manpath}:${OPT_PHENIX}/man
endif

if (-d ${OPT_PHENIX}/share/man) then
    set manpath = ${manpath}:${OPT_PHENIX}/share/man
endif

foreach d (${OFFLINE_MAIN}/bin ${ROOTSYS}/bin)
  if (-d $d) then
    set path = ($path $d)
  endif
end

set rootlibdir_tmp = `root-config --libdir`

if (-d $rootlibdir_tmp) then
  set here=`pwd`
  cd $rootlibdir_tmp
  set there=`pwd`
  set rootlibdir = `echo $there | sed "s/@sys/$sysname/g"`
  cd $here
endif

set ldpath = .

# this to keep the old the SL3 ana builds working
# they need libs from the SL3 /opt/phenix
set offlineversion=`basename $OFFLINE_MAIN`
set mainversion = `echo $offlineversion | awk -F. '{print $1}'`
# strip non numeric chars from minor version (handle e.g. pro.80swap)
set minorversion = `echo $offlineversion | awk -F. '{print $2}' |  sed 's/[^0-9]//g'`
set optphenix = "${OPT_PHENIX}/lib:${OPT_PHENIX_UTILS}/lib"
set optphenix64 = "${OPT_PHENIX}/lib64:${OPT_PHENIX_UTILS}/lib64:/opt/phenix/64/core/lib"

if ($mainversion =~ "ana" && $minorversion !~ "") then
  if ($minorversion < 50) then
    set optphenix = "/afs/rhic.bnl.gov/i386_sl305/opt/phenix/lib"
    echo using SL3 /opt/phenix under $optphenix
  else if ($minorversion < 166) then
    set optphenix = "/afs/rhic.bnl.gov/i386_sl4/opt/phenix/lib"
    echo using SL4 /opt/phenix under $optphenix
  else if ($minorversion < 379) then
    set optphenix = "/afs/rhic.bnl.gov/x8664_sl5/opt/phenix/lib"
    set  optphenix64 = "/afs/rhic.bnl.gov/x8664_sl5/opt/phenix/64/lib"
    echo using SL5 /opt/phenix under $optphenix and $optphenix64
    setenv ODBCINI /afs/rhic.bnl.gov/x8664_sl5/opt/phenix/etc/odbc.ini
  else if ($minorversion < 591) then
    set optphenix = "/afs/rhic.bnl.gov/x8664_sl6/opt/phenix/lib"
    set optphenix64 = "/afs/rhic.bnl.gov/x8664_sl6/opt/phenix/64/lib"
    echo using SL6.4 /opt/phenix under $optphenix and $optphenix64
    setenv ODBCINI /afs/rhic.bnl.gov/x8664_sl6/opt/phenix/etc/odbc.ini
  endif
endif

if ($mainversion =~ "pro" && $minorversion !~ "") then
  if ($minorversion =~ "66c" || $minorversion < 78) then
    set optphenix = "/afs/rhic.bnl.gov/i386_sl305/opt/phenix/lib"
    echo using SL3 /opt/phenix under $optphenix
  else if ($minorversion < 83) then
    set optphenix = "/afs/rhic.bnl.gov/i386_sl4/opt/phenix/lib"
    echo using SL4 /opt/phenix under $optphenix
  else if ($minorversion < 98) then
    set optphenix = "/afs/rhic.bnl.gov/x8664_sl5/opt/phenix/lib"
    set optphenix64 = "/afs/rhic.bnl.gov/x8664_sl5/opt/phenix/64/lib"
    echo using SL5.3 /opt/phenix under $optphenix and $optphenix64
    setenv ODBCINI /afs/rhic.bnl.gov/x8664_sl5/opt/phenix/etc/odbc.ini
  else if ($minorversion < 110) then
    set optphenix = "/afs/rhic.bnl.gov/x8664_sl6/opt/phenix/lib"
    set optphenix64 = "/afs/rhic.bnl.gov/x8664_sl6/opt/phenix/64/lib"
    echo using SL6.4 /opt/phenix under $optphenix and $optphenix64
    setenv ODBCINI /afs/rhic.bnl.gov/x8664_sl6/opt/phenix/etc/odbc.ini
  endif
endif

set ldpath = ${ldpath}:$optphenix

foreach d (/usr/local/lib /usr/lib \
           ${OFFLINE_MAIN}/lib ${rootlibdir} ${SIMULATION_MAIN}/lib)
  if (-d $d) then
   set ldpath = ${ldpath}:${d}
  endif
end

# Set up Insure++, if we have it
if (! $?PARASOFT) then
  set parasoft = /afs/rhic.bnl.gov/app/insure-7.5.5
  setenv PARASOFT $parasoft
  if (-d ${parasoft}/bin) then
    set path = ($path ${parasoft}/bin)
    set ldpath = ${ldpath}:${parasoft}/lib
  endif
else
  if (-d ${PARASOFT}/bin) then
    set path = ($path ${PARASOFT}/bin)
    set ldpath = ${ldpath}:${PARASOFT}/lib
  endif
endif

#add coverity
if (! $?COVERITY_ROOT) then
  setenv COVERITY_ROOT /afs/rhic.bnl.gov/app/coverity-2019.03
endif
if (-d $COVERITY_ROOT/bin) then
  set path = ($path  $COVERITY_ROOT/bin)
endif

# Java, if we have it
if ( $?JAVAHOME) then
    set path = ($path ${JAVAHOME}/bin) 
    if (! $?CLASSPATH) then
        setenv CLASSPATH .:${JAVAHOME}/lib
    endif
endif

if (! $?ONLINE_MAIN) then
      setenv ONLINE_MAIN $OFFLINE_MAIN
endif

if (-f $ONLINE_MAIN/bin/setup.com) then
    source $ONLINE_MAIN/bin/setup.com
    
    set path = ($path ${ONLINE_MAIN}/bin) 

    if (-d $ONLINE_MAIN/lib) then
      set ldpath = ${ldpath}:${ONLINE_MAIN}/lib
    endif

endif

# Add CERNLIB.
if (-d ${CERN}/${CERN_LEVEL}) then
    if (-d ${CERN}/${CERN_LEVEL}/bin) then    
	set path = ($path ${CERN}/${CERN_LEVEL}/bin) 
    endif	

    if (-d ${CERN}/${CERN_LEVEL}/lib) then    
	set ldpath = ${ldpath}:${CERN}/${CERN_LEVEL}/lib
    endif	

endif

# File catalog search path
if (! $?GSEARCHPATH) then
    setenv GSEARCHPATH .:PG
endif

#lhapdf path to pdf's
#if (! $?LHAPATH) then
#    if (-d /opt/phenix/share/lhapdf/PDFsets) then
#	setenv LHAPATH /opt/phenix/share/lhapdf/PDFsets
#    endif
#endif

#add our python packages and path to ROOT.py
if (! $?PYTHONPATH) then
  setenv PYTHONPATH ${OPT_PHENIX}/pythonpackages/lib/python3.8/site-packages:${ROOTSYS}/lib
endif

# Add Geant4
if (! $?G4_MAIN) then
    if (-d $OFFLINE_MAIN/geant4) then
      setenv G4_MAIN $OFFLINE_MAIN/geant4
    else
      setenv G4_MAIN ${OPT_PHENIX}/geant4
    endif
endif


if (-d $G4_MAIN) then
# normalize G4_MAIN to /opt/phenix/geant4.Version
    set here=`pwd`
    cd $G4_MAIN
    set there=`pwd`
    setenv G4_MAIN `echo $there | sed "s/@sys/$sysname/g"`
    cd $here
# this is for later possible use, extract the main version number
    set g4basedir = `basename $G4_MAIN`
    set g4mainversion = `echo $g4basedir | awk -F. '{print $2}'`
    if (-f ${G4_MAIN}/geant4.csh) then
         source ${G4_MAIN}/geant4.csh >& /dev/null
    else
        if (-f ${G4_MAIN}/bin/geant4.csh) then
            set here=`pwd`
            cd $G4_MAIN/bin
            source geant4.csh  >& /dev/null
            cd $here
        endif
    endif
    if (-d ${G4_MAIN}/bin) then
	set  path = ($path ${G4_MAIN}/bin)
    endif
    if (-d ${G4_MAIN}/lib) then
	set  ldpath = ${ldpath}:${G4_MAIN}/lib
    endif
endif

# add 64 bit /opt/phenix for perl
set ldpath = ${ldpath}:${optphenix64}

# Set some actual environment vars
if ($opt_a) then
    setenv PATH ${ORIG_PATH}:${PATH}
    if (! $?LD_LIBRARY_PATH) then
	setenv LD_LIBRARY_PATH ${ldpath}
    else
	setenv LD_LIBRARY_PATH ${ORIG_LD_LIBRARY_PATH}:${ldpath}
    endif
    if (! $?MANPATH) then
	setenv MANPATH ${manpath}
    else
	setenv MANPATH ${ORIG_MANPATH}:${manpath}
    endif
else
    setenv LD_LIBRARY_PATH ${ldpath}
    setenv MANPATH ${manpath}
endif


if (-f $OPT_PHENIX/bin/odbcini_setup.csh) then
    source $OPT_PHENIX/bin/odbcini_setup.csh
endif

#replace @sys by afs sysname (to strip duplicate entries with /@sys/ and /x86_64_sl7/)
setenv PATH  `echo $PATH | sed "s/@sys/$sysname/g"`
setenv LD_LIBRARY_PATH `echo $LD_LIBRARY_PATH | sed "s/@sys/$sysname/g"`
setenv MANPATH  `echo $MANPATH | sed "s/@sys/$sysname/g"`

# strip duplicates in paths
setenv PATH `echo -n $PATH | awk -v RS=: -v ORS=: '! arr[$0]++'` 
setenv LD_LIBRARY_PATH `echo -n $LD_LIBRARY_PATH | awk -v RS=: -v ORS=: '! arr[$0]++'`
setenv MANPATH `echo -n $MANPATH | awk -v RS=: -v ORS=: '! arr[$0]++'`
# the above leaves a colon at the end of the strings, so strip the last character
setenv PATH `echo -n $PATH | sed 's/.$//'`
setenv LD_LIBRARY_PATH `echo -n $LD_LIBRARY_PATH | sed 's/.$//'`
setenv MANPATH `echo -n $MANPATH | sed 's/.$//'`
