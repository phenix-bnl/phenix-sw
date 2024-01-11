void update_attenuated_channels()
{
printf("Loading PHOOL libraries\n");
  gSystem->Load("libfun4all.so");


  /*  This database file will likely never need modification.
      The parameters below were used in the initial commit.
      One update to the original commit was made.

      The arms do not need to be created from the database in order 
      to store the new file!  It saves time not to call the database 
      arm constructor via  MutArm(armNum, timeStamp)

      This function stores the channels for each arm independently.
      Since there were no entries for the South arm, it's commented 
      out.
  */

  //used for 
  PHTimeStamp Tstart(2001,6,1,0,0,0,0);
  PHTimeStamp Tstop;
  Tstop.setToFarFuture();

  char *descrip = "attenuated strips";
  char *fileName = "AttenuatedChannels.txt";

  PdbBankID bankID = 0;
  //  MutArm *s = new MutArm(0);
  MutArm *n = new MutArm(1);

  //confirmation that everything is going well...
  //  s->print();
  n->print();

  n->updateAttenuatedChannels(Tstart, Tstop, bankID, descrip, fileName);

  //  delete s;
  delete n;
}












