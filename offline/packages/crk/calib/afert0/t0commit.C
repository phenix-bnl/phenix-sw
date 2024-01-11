{
gSystem.Load("libndst");

PHTimeStamp tStart(2002,4,30,0,0,0);
PHTimeStamp tStop(2003,7,1,0,0,0);

ifstream fin("Run3dAuT0.txt");

Int_t channel;
Float_t t0[5120];
Float_t t0sigma[5120];

for(int ipmt=0;ipmt<5120;ipmt++){
  fin >> channel >> t0[ipmt] >> t0sigma[ipmt];
}
fin.close();

PdbADCChan* achan = 0;
const char* calibname = "afterburner.crk.tzero";
const char* description = "rich t0 for afterburner";

PdbBankManager *bankManager = PdbObjyBankManager::instance();
PdbApplication *application = bankManager->getApplication();
PdbBankID bankID("");
bankID.setInternalValue(0);

if(application->startUpdate()) {

      PdbCalBank *Bank = bankManager->createBank("PdbADCChanBank",
                                                 bankID,
                                                 description,
                                                 tStart, tStop,
                                                 calibname);


       Bank->setLength(5120);
      for(int i=0; i<5120; i++) {
        achan = (PdbADCChan*)&(Bank->getEntry(i));
        achan->setParameter(0, t0[i]);
        achan->setParameter(1, t0sigma[i]);
      }
      delete Bank;
    }

        application->commit();

}
}
