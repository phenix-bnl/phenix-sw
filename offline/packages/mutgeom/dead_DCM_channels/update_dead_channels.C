void update_dead_channels()
{
printf("Loading PHOOL libraries\n");
  gSystem->Load("libfun4all.so");


  /*  You need to modify the start and stop times that the 
      channels were dead.
      Modify descrip to give a brief description of the database 
      file or update.
      Modify fileName to the file you need to insert into the database.
      bankdID is 0 unless you want different versions for the same 
      time period.

      The arms do not need to be created from the database in order 
      to store the new file!  It saves time not to call the database 
      arm constructor via  MutArm(armNum, timeStamp)

      This function stores the dead channels for each arm independently.
      So if only one arm needs to be modified, remove the calls to 
      the other.
  */

  //used for last update
  PHTimeStamp Tstart(2003,4,28,0,0,0,0);
  PHTimeStamp Tstop(2003,6,29,0,0,0,0);

  // for reference:
  //PHTimeStamp Tstop;
  //Tstop.setToFarFuture();

  char *descrip = "run 3 dead channels after Apr 28, 03";
  char *fileName = "DeadChannelsRun3afterApril28.dat";

  PdbBankID bankID = 0;
  MutArm *s = new MutArm(0);
  MutArm *n = new MutArm(1);

  //confirmation that everything is going well...
  s->print();
  n->print();

  s->updateDeadChannels(Tstart, Tstop, bankID, descrip, fileName);
  n->updateDeadChannels(Tstart, Tstop, bankID, descrip, fileName);

  delete s;
  delete n;
}












