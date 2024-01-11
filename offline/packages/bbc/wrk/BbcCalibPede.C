void BbcCalibPede(Int_t maxEvents=1, Int_t verbose=1) {

  Int_t eventNumber = 0;

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
  PHString inputFile = "phnx.prdf";

  // Set up the main iterator and event
  //mainIter.cd();
  //Event *thisEvent = 0;
  //mainIter.addNode(new PHDataNode<Event>(thisEvent, "PRDF"));
  PHNodeReset reset;
  //Eventiterator *eventIter = new fileEventiterator(inputFile.getString());
  
  Int_t kevent = 0;  // counts number of PISA events processed

  BbcCalib* bbccalib = new BbcCalib();
  bbccalib->restore();
  //
  // Set BbcEvent object
  //
  BbcEvent* bbcevent = new BbcEvent();
  bbcevent->setCalibDataAll( bbccalib );
  //
  //
  //
  CalibratorPeak pedecalib;
  CalibratorPeak overflow0;
  CalibratorPeak overflow1;
  PHTimeStamp time   = PHTimeStamp(2000,5,1,0,0,0);
  pedecalib.Initualize(time,"pedestal");
  overflow0.Initualize(time,"overflow0");
  overflow1.Initualize(time,"overflow1");
  //
  // Open the output DST file
  //
  PHNodeIOManager *ioDST = new PHNodeIOManager("DSTEval.root", PHWrite);

  gROOT->cd();

  while ( eventNumber++ < maxEvents) {
      cout << "\n Fetched event " << eventNumber << endl;


    // Point the data node to the new event
    //mainIter.cd();
    //((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);

    bbcevent->setEventNumber( eventNumber );
    //
    // Set raw data for REAL event
    //bbcevent->setRawData( topNode ); 
    //
    // Set raw data for Fake Pedestal event
    bbcevent->setFakePedestal();
    bbcevent->calculate();
    pedecalib->FillHist( bbcevent, "pedestal"); 
    overflow0->FillHist( bbcevent, "overflow0"); 
    overflow1->FillHist( bbcevent, "overflow1"); 

    // Reset all data for this event 
    mainIter.cd();
    if (mainIter.cd("DST")) {
      // cout << "\n In DST " << endl;
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("DCM")) {
      // cout << "\n In DCM " << endl;
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("BBC")) {
      // cout << "\n In BBC " << endl;
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("EVA")) {
      // cout << "\n In EVA " << endl;
      mainIter.forEach(reset);
      mainIter.cd();
    }

  } // loop over events

  pedecalib->Calculate();
  overflow0->Calculate();
  overflow1->Calculate();

  pedecalib->Evaluate();
  overflow0->Evaluate();
  overflow1->Evaluate();

  PHTimeStamp now    = PHTimeStamp(2000,5,2,0,0,0);
  pedecalib->StoreToDB(now,"pedestal");
  overflow0->StoreToDB(now,"overflow0");
  overflow1->StoreToDB(now,"overflow1");
}

