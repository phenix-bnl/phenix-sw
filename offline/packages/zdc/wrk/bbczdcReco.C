void bbczdcReco(Int_t maxEvents=0, Int_t verbose=0) {

  Int_t eventNumber = 0;

  //
  // Set up the PHOOL initialization
  //
  gROOT->Macro("phoolRecoInit.C");

  //
  // Set up the PISA interface initialization
  //
  gROOT->Macro("pisaRecoInit.C");

  // Executing initialization and parameter macros
  gROOT->Macro("bbctestini.C");
  gROOT->Macro("bbctestpar.C");
  //
  TH2F* zdcbbc = new TH2F("bbczdc","",150,-150,150,150,-150,150);

  //
  // BBC setup module calls
  //
  mBbcSetGeo->event(topNode);

  mZdcEvent->setEventNumber(0); 
  mZdcEvent->Clear(); 

  // Set up the main iterator and event
  mainIter.cd();
  Event *thisEvent = 0;
  mainIter.addNode(new PHDataNode<Event>(thisEvent, "PRDF"));
  PHNodeReset reset;
  Eventiterator *eventIter = new fileEventiterator(inputFile.getString());

  //
  // Open the output DST file
  //
  PHNodeIOManager *ioDST = new PHNodeIOManager("DST.root", PHWrite);

  gROOT->cd();

  while ((thisEvent = eventIter->getNextEvent()) && eventNumber++ < maxEvents) {

    if(eventNumber<=verbose) {
      cout << "\n Fetched event " << eventNumber << endl;
    }
      cout << "\n Fetched event " << eventNumber << endl;

    // Point the data node to the new event
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);

    BbcGetDCM(topNode);
    ZdcGetDCM(topNode);
    if(eventNumber<=verbose) {
      dBbcDCM->Show();
    }

    mBbcUnpack->event(topNode);
    if(eventNumber<=verbose) {
      dBbcRaw->Show();
    }

    mBbcRawOut->event(topNode);
    if(eventNumber<=verbose){
      dBbcOut->Show(); 
      cout << "\n";
    }
    mZdcEvent->setEventNumber(eventNumber);
    mZdcEvent->Clear();
    mZdcEvent->DcmToRaw(topNode);
    mZdcEvent->PutEndProduct(topNode);
    //
    cout << "Zvtx = " << dBbcOut->get_VertexPoint(0) << " " << dZdcOut->get_Zvertex(0) << endl;

    zdcbbc->Fill( dBbcOut->get_VertexPoint(0),dZdcOut->get_Zvertex(0));

    //
    // write out the DST
    //
    ioDST->write(dstNode);

    // Reset all data for this event
    mainIter.cd();
    if (mainIter.cd("DST")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("DCM")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("BBC")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("ZDC")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("EVA")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }

  } // loop over events

  delete ioDST;  // this actually closes the output DST file

}

