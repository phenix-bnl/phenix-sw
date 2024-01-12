void pidrecal_update()
{
  //  This routine puts constants into the database by hand.
  // - it was valid for Run4 (run 109822 - 130553), T.Chujo (Oct. 19, 2004)
  // - make it valid for early runs too (run 105218 - 130553), T.Chujo (Nov. 07, 2004)

  gSystem->Load("libfun4all");
  PidrecalReco *pid = new PidrecalReco("HWGCentralTrack");

  pid->fetchPidParFromFile("pidrecal_par.txt");
  /*
  pid->PidPar[0] = 0.019875;   // m2tof mean pion
  pid->PidPar[1] = 0.2509; // m2tof mean kaon
  pid->PidPar[2] = 0.8923; // m2tof mean proton
  pid->PidPar[3] = 1.32;       // Sigma alpha
  pid->PidPar[4] = 1.24;       // Sigma ms
  pid->PidPar[5] = 0.120;     // Sigma tof
  pid->PidPar[6] = 101;       // K1
  */
  pid->Print();
  
  // used unix time: 
  // start time : 1/19/2004, 00:00:00 = unix time 1074470400
  // end time   : 1/1/2010,  00:00:00 = unix time 1262304000
  //  pid->updatePidPar(1074470400,1262304000);
  //  pid->updatePidPar(109822,130553); // for Run4
  //  pid->updatePidPar(105218,130553); // for Run4 included early runs. 
  pid->updatePidPar(146651);     // for Run5 and future (run 146651 -) 


}
