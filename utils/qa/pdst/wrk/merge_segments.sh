#! /bin/sh -f
production_tag=${1}
rm -f run9pp200_*.list
rm -f run9pp200.list
cat run9pp200.list | while read line
do
    run=`echo ${line} | awk -F- '{print $(NF-1)}'`
    echo ${line} >> run9pp200_${run}.list
done
for line run9pp_200_*.list
do
  run=`echo ${line} | cut -d_ -f3 | cut -d. -f1`
  hmerge ${line} ${production_tag}/agg/qa_${production_tag}-${run}.root
  echo ${production_tag}/agg/qa_${production_tag}-${run}.root >> run9pp200.list
done
rm -f run9pp200_*.list
