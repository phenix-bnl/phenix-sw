#/bin/csh

# in condor people can use Environment=True which carries their
# complete environment to the condor job including the
# ODBCINI variable. So we need to unset it in case it was set
# to our defaults
set odbcinifile1 = ${OPT_PHENIX}/etc/odbc.ini.phnxdbrcf1
set odbcinifile2 = ${OPT_PHENIX}/etc/odbc.ini.phnxdbrcf2
set odbcinifile4 = ${OPT_PHENIX}/etc/odbc.ini.phnxdbrcf4
set odbcinifile5 = ${OPT_PHENIX}/etc/odbc.ini.phnxdbrcf5
if ($?ODBCINI) then 
  if ( $ODBCINI =~ $odbcinifile1) then
    unsetenv ODBCINI
  endif
endif
if ($?ODBCINI) then
  if ( $ODBCINI =~ $odbcinifile2) then
    unsetenv ODBCINI
  endif
endif

#only set ODBCINI if not set before by user
if (! $?ODBCINI) then
# set ODBCINI, use even/odd ip addresses to distribute DB load
# between odbcini files
  set modulo = `hostname -i | awk -F. '{print $4%4}'`
  switch($modulo)
    case 0:
      setenv ODBCINI $odbcinifile1
      breaksw
    case 1:
      setenv ODBCINI $odbcinifile2
      breaksw
    case 2:
      setenv ODBCINI $odbcinifile4
      breaksw
    case 3:
      setenv ODBCINI $odbcinifile5
      breaksw
  default:
      setenv ODBCINI $odbcinifile5
      breaksw
  endsw
endif

