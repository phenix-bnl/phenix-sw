//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void bbcrun(Int_t maxEvents) {

  TH1F *bbczvtx = new TH1F("bbczvtx"," Z vertex form BBC",100,-200, 200);

  Int_t eventNumber = 0;

  // Executing initialization and parameter macros
  gROOT->Macro("bbcini.C");
  gROOT->Macro("bbcpar.C");

  // Set up input and output files
  PHString inputFile = "/phenix/data06/evt_data/rc-0005434-00-atp.0-P01.prdf";

  // Set up the main iterator and event
  mainIter.cd();
  Event *thisEvent = 0;
  mainIter.addNode(new PHDataNode<Event>(thisEvent, "PRDF"));
  PHNodeReset reset;
  Eventiterator *eventIter = new fileEventiterator(inputFile.getString());


  if (verbose>5) printf("Entering event loop.\n");

  gROOT->cd();

  while ((thisEvent = eventIter->getNextEvent()) && eventNumber++ < maxEvents) {

    if (verbose>5) printf("Fetched event %d\n",eventNumber);

    // Point the data node to the new event
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);

    printf("Calling first event only modules.\n");
    if (eventNumber == 1) {
      if (verbose>10) printf("Calling mBbcSetGeo\n");
      mBbcSetGeo->event(topNode);
    }

    if (verbose>10) printf("Calling event modules\n");

    if (verbose>10) printf("Calling BbcGetDCM\n");
    //BbcGetDCM(topNode);

    if (verbose>10) printf("Calling mBbcUnpack\n");
     mBbcUnpack->event(topNode);

    if (verbose>10) printf("Calling mBbcRawOut\n");
     mBbcRawOut->event(topNode);

    cout << dBbcOut->get_VertexPoint(0) << endl;
    bbczvtx->Fill( dBbcOut->get_VertexPoint(0) );

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
  }

}

