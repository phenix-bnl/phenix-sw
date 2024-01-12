#!/bin/csh -f
# script to submit qa summary jobs to lsf
# 1th parameter : runnumber file list
# 2th parameter : production tag name
production_tag=$2
if [ -d summary_${produtcion_tag} ]
   then
   else
      mkdir summary_${produtcion_tag}
fi
if [ -d status_${produtcion_tag} ]
    then
    else
        mkdir status_${produtcion_tag}
fi
if [ -d LOG_${produtcion_tag} ]
    then
    else
        mkdir LOG_${produtcion_tag}
fi
if [ -d ERR_${produtcion_tag} ]
    then
    else
        mkdir ERR_${produtcion_tag}
fi
cat $1 |
  while read line; do
   bsub -q phenix_cas -o out_${line}.log -e err_${line}.log sh merge_segments.sh $line $production_tag
#  sh merge_segments.sh $line $production_tag >& out_${line}.log
   done

