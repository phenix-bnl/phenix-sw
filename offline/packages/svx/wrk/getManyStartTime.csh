#!/bin/csh

set runfile = $1

if (-r $runfile) then

  foreach run (`cat $runfile`)
    ./getStartTime.csh $run
  end
else
  echo "stop -- $file doesn't exit "
endif

