{
gSystem.Load("libndst");

// date to be taken from
PHTimeStamp sch(2003,2,2,0,0,0);

// output file
ofstream fout("t0aftcal_run3.txt");


PdbADCChan* achan = 0;
char* calibname = "afterburner.crk.tzero";
char* description = "rich t0 for afterburner";
PdbBankManager *bankManager = PdbObjyBankManager::instance();
PdbApplication *application = bankManager->getApplication();
PdbBankID bankID("");
bankID.setInternalValue(0);



if(application->startRead()) {

  PdbCalBank *Bank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, sch);

    if(Bank) {
      int banklength = Bank->getLength();
      for(int i=0; i<banklength; i++) {
        achan = (PdbADCChan*)&(Bank->getEntry(i));
	fout << i << " " << achan->getParameter(0) << " "
	     << achan->getParameter(1) << endl;
      }
      delete Bank;
    }
    fout.close();

}
}
