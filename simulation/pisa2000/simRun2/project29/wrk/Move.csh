echo " Moving ana.root ... "
mv *_ana.root Ana/

echo " Moving pi0sim.root ... "
mv *_pi0sim.root Pi0sim/

echo " Moving log.txt.gz ... "
mv *_log.txt.gz Log/

echo " Moving job.log ... "
mv job.*.log dir_job/

echo " Moving job.err ... "
mv job.*.err dir_job/

echo " Finished... "

