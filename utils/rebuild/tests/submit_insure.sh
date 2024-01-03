#!/bin/sh
if [ ! -n "$1" ]
then
  echo "need prdf filename with full path as argument"
  exit 2
fi
# set up directory structures
workdir=`pwd`
insureoutdir="insureout"
insurecondordir="insurecondor"
insurescript="insure.csh"
for outdir in $insureoutdir $insurecondordir; do
if [ -d $outdir ]; then
  rm -rf $outdir
fi
mkdir $outdir
done

for macro in valgrind_*.C; do
macrobase=`basename $macro .C`
cat > $insurecondordir/$macrobase.job <<EOF
Executable     =  $workdir/$insurescript
Universe = vanilla
Notification = Error
GetEnv          = True
+Experiment  = "phenix"
+Job_Type = "cas"
output  = $workdir/$insureoutdir/$macrobase.log
error   = $workdir/$insureoutdir/$macrobase.log
Log     = $workdir/$insureoutdir/$macrobase.log

Initialdir     = $workdir

Arguments = $workdir/$macro $1


Queue
 
EOF
condor_submit $insurecondordir/$macrobase.job  2>&1
done
