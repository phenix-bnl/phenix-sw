void bbcSpecial(Int_t maxEvents=1, Int_t verbose=1) {

  Int_t eventNumber = 0;

  //
  // Loading PHOOL libraries
  //
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea.so");

  //
  // Loading BBC libs
  //
  gSystem->Load("libbbc_tables.so");
  gSystem->Load("libbbc.so");

  // Set up input PRDF
  PHString inputFile = "/phenix/data06/evt_data/rc_0005757.prdfz";

  Event *thisEvent = 0;
  Eventiterator *eventIter = new fileEventiterator(inputFile.getString());

  PHTimeStamp tSearch = PHTimeStamp(2000,4,2,0,0,0);
  BbcCalib *bbccalib;
  bbccalib = new BbcCalib();
  bbccalib->restore("BbcCalib");

  BbcEvent bbcevent;
  bbcevent.setCalibDataAll(bbccalib);

  Packet* p;

  while ((thisEvent = eventIter->getNextEvent()) && eventNumber++ < maxEvents) {
     if ( eventNumber == 1 ) goto next;
     if ( (p = thisEvent->getPacket(id)) != 0) {
       cout << iValue(0) << " " << iValue(0,"T1") << " "  << endl;
       cout << iValue(1) << " " << iValue(1,"T1") << " "  << endl;
       cout << iValue(2) << " " << iValue(2,"T1") << " "  << endl;
       cout << iValue(3) << " " << iValue(3,"T1") << " "  << endl;
       cout << iValue(4) << " " << iValue(4,"T1") << " "  << endl;
       cout << iValue(5) << " " << iValue(5,"T1") << " "  << endl;
       cout << iValue(6) << " " << iValue(6,"T1") << " "  << endl;
       cout << iValue(7) << " " << iValue(7,"T1") << " "  << endl;
     }
     bbcevent.setRawData( thisEvent );
     bbcevent.calculate();
     cout << bbcevent.getTimeZero() << " " << bbcevent.getZVertex() << endl;    
     next:;
  } // loop over events
  
}

