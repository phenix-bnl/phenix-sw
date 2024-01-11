#!/bin/csh

set run = $1

root -b -l <<EOF 
  gSystem->Load("libfun4all");
  RunToTime *rtt = RunToTime::instance()
  PHTimeStamp *phts = rtt->getBeginTime($run)
  int time = (int) phts->getTics();
  cout << $run << " " << time << endl;
  .q
EOF
