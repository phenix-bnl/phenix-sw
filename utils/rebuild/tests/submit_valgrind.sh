#!/bin/sh
if [ ! -n "$1" ]
then
  echo "need prdf filename with full path as argument"
  exit 2
fi

# set up directory structures
workdir=`pwd`
valgrindoutdir="valgrindout"
valgrindcondordir="valgrindcondor"
valgrindscript="valgrind.csh"
for outdir in $valgrindoutdir $valgrindcondordir; do
if [ -d $outdir ]; then
  rm -rf $outdir
fi
mkdir $outdir
done

for macro in valgrind_*.C; do
macrobase=`basename $macro .C`
cat > $valgrindcondordir/$macrobase.job <<EOF
Executable     =  $workdir/$valgrindscript
Universe = vanilla
Notification = Error
GetEnv          = True
+Experiment  = "phenix"
+Job_Type = "cas"
output  = $workdir/$valgrindoutdir/$macrobase.log
error   = $workdir/$valgrindoutdir/$macrobase.log
Log     = $workdir/$valgrindoutdir/$macrobase.log

Initialdir     = $workdir

Arguments = $workdir/$macro $1


Queue
 
EOF
condor_submit $valgrindcondordir/$macrobase.job  2>&1
done
