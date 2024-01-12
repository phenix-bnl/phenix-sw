void generic_initialization()
{
  //  This routine puts constants into the database by hand.
  //  The grr constructor zeroes all the constants, but I made
  //  one of them non-zero for fun...

  gSystem->Load("libfun4all");
  GenericrecalReco *grr = new GenericrecalReco("HWGCentralTrack");
  grr->setRecalData(0,1,0.5); //FUN!
  grr->Print();
  grr->update(1000,2000);
  delete grr;
}
