void pidrecal_init()
{
  //  This routine puts constants into the database by hand.
  // it was valid for only 62.4 GeV data (run 122470 -- 123564)
  // extended validity range from Run 100000 to 200000. (Aug. 27, 2004) T.Chujo

  gSystem->Load("libfun4all");
  PidrecalReco *pid = new PidrecalReco("HWGCentralTrack");
  pid->PidPar[0] = 0.019875;   // m2tof mean pion
  pid->PidPar[1] = 0.2509; // m2tof mean kaon
  pid->PidPar[2] = 0.8923; // m2tof mean proton
  pid->PidPar[3] = 1.32;       // Sigma alpha
  pid->PidPar[4] = 1.24;       // Sigma ms
  pid->PidPar[5] = 0.120;     // Sigma tof
  pid->PidPar[6] = 101;       // K1
  pid->Print();
  pid->updatePidPar(100000,200000);

}
