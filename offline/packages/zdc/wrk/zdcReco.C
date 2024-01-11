void zdcReco(Int_t maxEvents=0, Int_t verbose=0) {

  Int_t eventNumber = 0;

  //
  // Set up the PHOOL initialization
  //
  gROOT->Macro("phoolRecoInit.C");

  // Executing initialization and parameter macros
  gROOT->Macro("zdcini.C");
  gROOT->Macro("zdcpar.C");
  //

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

    ZdcGetDCM(topNode);

    mZdcEvent->setEventNumber(eventNumber);
    mZdcEvent->Clear();
    mZdcEvent->DcmToRaw(topNode);
    mZdcEvent->PutEndProduct(topNode);
    //
    cout << "Zvtx   = " << dZdcOut->get_Zvertex(0) << " ";
    cout << "Energy = " << dZdcOut->get_Energy(0,0)  << " ";
    cout << "Energy = " << dZdcOut->get_Energy(1,0)  << endl;

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

