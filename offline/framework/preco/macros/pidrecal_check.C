void pidrecal_check(int run = 122470)
{
  //  This routine displays constants from the database,.

  gSystem->Load("libfun4all");
  PidrecalReco *pid = new PidrecalReco("HWGCentralTrack");
  pid->fetchPidPar(run);
  pid->Print();
}
