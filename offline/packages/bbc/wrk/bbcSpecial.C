void bbcSpecial(Int_t maxEvents=100, Int_t verbose=1) {

  TH1F *zvtx = new TH1F("zvtx","Bbc ZVertex", 160,-160.0,160.0);
  TH1F *t0   = new TH1F("t0",  "Bbc TimeZero",100, -10.0, 10.0);

  TH1F *adc  = new TH1F("adc","adc distribution",128,0.,4096);

  Int_t eventNumber = 1;

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
  gSystem->Load("libphgeo.so");

  //
  // Loading BBC libs
  //
  gSystem->Load("libbbc.so");

  // Set up input PRDF
  //PHString inputFile = "/phenix/data06/evt_data/EVENTDATAxxx_P01-0000010327-0000.PRDFF";
  //PHString inputFile = "/phenix/data22/muMDC/prdf/SIMULA_0010200225-0001.PRDFF";
  PHString inputFile = "/phenix/workarea/ohnishi/cvs/genprdf/phnx.prdf";
  //PHString inputFile = "/direct/phenix+data07/frawley/test6.prdf";
  //PHString inputFile = "/phenix/data24/chiu/L2prdf/phnx1.prdf";

  int status;
  Event *thisEvent = 0;
  Eventiterator *eventIter = new fileEventiterator(inputFile.getString(),status);

  PHTimeStamp tSearch = PHTimeStamp(2000,8,2,0,0,0);
  BbcCalib *bbccalib;
  bbccalib = new BbcCalib();

//bbccalib->setSimulation(1);
//bbccalib->getSimulation();

  //bbccalib->restore(tSearch);
  bbccalib->restore("/phenix/workarea/ohnishi/Calib/bbc/data/calib_par_061001/09438/BbcCalib");
  //bbccalib->restore();
  bbccalib->showParameters();

  BbcEvent bbcevent;
  bbcevent.setCalibDataAll(bbccalib);

  cout.precision(3);
  cout.setf(ios::fixed);  

  Packet *p;
  while ((thisEvent = eventIter->getNextEvent())  ) {

//   PHTimeStamp *time0 = thisEvent->getTimeStamp();
//   time0->print();

     if ( eventNumber == 1 ) goto next;
     if ( (p = thisEvent->getPacket(1001))!=0) {
       for (int i=0;i<128;i++){
//         cout << i << " " << p->iValue(i) << " " << p->iValue(i,"T1") << " " << p->iValue(i,"T2") << endl; 
           adc->Fill((float)(p->iValue(i)));
       }  
     }
     delete p;
     bbcevent.setRawData( thisEvent );
     for ( int i=0;i<128;i++ ){
//     cout << i << " " << bbcevent.getAdc(i) << " " << bbcevent.getTdc0(i) << " " << bbcevent.getTdc1(i) << endl;
     }
     bbcevent.calculate();
     if ( eventNumber%100 == 0 ) {
//     cout << "T0 = " << bbcevent.getTimeZero() << " Bbc Zvtx = " << bbcevent.getZVertex() << endl;    
     }
     cout << eventNumber << " " << bbcevent.getTimeZero() << " " << bbcevent.getZVertex() << " " ;
     cout << bbcevent.getnHitPmt(0) << " " << bbcevent.getnHitPmt(1) << endl;

     zvtx->Fill(bbcevent.getZVertex());
     t0->Fill(bbcevent.getTimeZero());
     next:;
     eventNumber++;
  } // loop over events
  cout << eventNumber << " events processed" << endl;
}

