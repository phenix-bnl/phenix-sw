void BbcCalibSlew(Int_t maxEvents=1, Int_t verbose=1) {

  //
  // Set up the PHOOL initialization
  //
  gROOT->Macro("phoolRecoInit.C");

  // Executing initialization and parameter macros
  gROOT->Macro("BbcCalibini.C");
  gROOT->Macro("BbcCalibpar.C");

  //
  // BBC setup module calls
  //
  mBbcSetGeo->event(topNode);

  // Set up input PRDF
  PHString inputFile = "/phenix/data06/evt_data/rc_0005775.prdfz";

  Event *thisEvent = 0;
  Eventiterator *eventIter = new fileEventiterator(inputFile.getString());

  Int_t kevent = 0;  // counts number of PISA events processed

  PHTimeStamp time   = PHTimeStamp(2000,5,5,0,0,0);
  BbcCalib* bbccalib = new BbcCalib();
  bbccalib->restore("BbcCalib");
  //
  // Set BbcEvent object
  //
  BbcEvent* bbcevent = new BbcEvent();
  bbcevent->setCalibDataAll( bbccalib );
  //
  //
  //
  CalibratorFitPar slewcalib;
  slewcalib.Initualize(time,"slewpar0");
  //
  // Open the output DST file
  //
  PHNodeIOManager *ioDST = new PHNodeIOManager("DSTEval.root", PHWrite);

  gROOT->cd();

  for ( int irun=0; irun<1; irun++ ) {
    cout << "\n Fetched Irun " << irun << endl;
    Int_t eventNumber = 0;
    slewcalib->ResetHist();
    while ( (thisEvent = eventIter->getNextEvent()) && eventNumber++ < maxEvents) {
      bbcevent->Clear();
      bbcevent->setRawData( thisEvent );
      bbcevent->calculate();

      if ( bbcevent->getnHitPmt(0)>50&&bbcevent->getnHitPmt(0)>50 ) {
         slewcalib->FillHist( bbcevent, "slewpar0"); 
      }
    } // loop over events

    slewcalib->Calculate( bbcevent );
  } // loop over runs

  //slewcalib->Evaluate();

  PHTimeStamp now    = PHTimeStamp(2000,5,2,0,0,0);
  //slewcalib->StoreToDB(now,"slewpar0");
}

