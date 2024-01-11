void read_Rich_align_database_sim() {
  //
  // YA   2002.11.20
  //
  // This macro can be used to read the RICH alignment data
  // in PdbCal database.
  // The database was implemented by Sasha Lebedev for QM2002 analysis.
  // The code below is copied his code in CrkGeometryObject::FetchReal()
  // Amazingly, just adding gSystem->Load("libndst.so"), one can
  // use the same code from ROOT prompt as macro.
  // I added few lines so that one can see the insert time, validity range,
  // etc of the fetched data.
  //
  //
  gSystem->Load("libndst.so");
  PdbADCChan* achan = 0;
  char* calibname = "geom.crk.run2";
  
  PdbBankManager *bankManager = PdbObjyBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbBankID bankID("");
  bankID.setInternalValue(0);

// RICH simulation alignment has validity range Jan 1st - Jan 31st 1999
  PHTimeStamp tSearch(1999,1,15,0,0,0);

  if(application->startRead()) {
    PdbCalBank *Bank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, tSearch);
    
    cout << "Insert time   :"<<Bank->getInsertTime() <<endl;
    cout << "Start val time:"<<Bank->getStartValTime() <<endl;
    cout << "End val time  :"<<Bank->getEndValTime() <<endl;
    cout << "Description   :"<<Bank->getDescription() <<endl;
    cout << "UserName      :"<<Bank->getUserName() <<endl;
    if(Bank) {
      int banklength = Bank->getLength();
      cout << banklength <<endl;
      for(int i=0; i<banklength; i++) {
	achan = (PdbADCChan*)&(Bank->getEntry(i));
	int tmp1 = (int)achan->getParameter(0);
	int arm = tmp1/1000;
	int side = (tmp1/100)%10;
	int mirror = tmp1%100;
	float tmp2 = achan->getParameter(1);
	float tmp3 = achan->getParameter(2);
	cout << arm <<" "<<side<<" "<<mirror<<" "<<tmp2<<" "<<tmp3<<endl;
      }
    }
    application->commit();
  }
}
