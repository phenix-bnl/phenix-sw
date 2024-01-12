void generic_check(int run)
{
  //  This routine displays constants from the database,.

  gSystem->Load("libfun4all");
  GenericrecalReco *grr = new GenericrecalReco("HWGCentralTrack");
  grr->fetch(run);
  grr->Print();
  delete grr;
}
