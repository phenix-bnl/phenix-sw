//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void foorun(const Int_t maxEvents=1, const char *prdfIFile="phnx.prdf", const char *dstOFile="centdst.root") {

  Int_t eventNumber = 0;

  // Executing initialization and parameter macros
  gROOT->Macro("fooini.C");
  gROOT->Macro("foopar.C");

  mainIter.cd();

  if (verbose>5) printf("Entering event loop.\n");
  while ((thisEvent = eventIter->getNextEvent()) && eventNumber++ < maxEvents) {

    // Point the data node to the new event
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);

    if (verbose>5) printf("Fetched event %d\n",eventNumber);


    printf("Calling first event only modules.\n");
    if (eventNumber == 1) {

      runNumber = thisEvent->getRunNumber();
      runDate = thisEvent->getDate();
      runTime = thisEvent->getTime();
      if (verbose>5) printf("Analyzing run number %d, date %d, time %d\n",runNumber,runDate,runTime);


      if (verbose>10) printf("Calling mBbcSetGeo\n");
      mBbcSetGeo->event(topNode);
 
    }

    if (verbose>10) printf("Calling event modules\n");

    if (verbose>10) printf("Calling BbcGetDCM\n");
    BbcGetDCM(topNode);

    if (verbose>10) printf("Calling mBbcUnpack\n");
     mBbcUnpack->event(topNode);

    if (verbose>10) printf("Calling mBbcRawOut\n");
     mBbcRawOut->event(topNode);
     cout << dBbcOut->get_VertexPoint(0) << endl;;

     dBbcRaw->Show();
     topNode->print();

    dstOut->write(dstNode);
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
    if (mainIter.cd("GEA")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("EVA")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("BBC")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
  }

  // Take out the garbage
  delete dstOut;
  if (verbose>10) printf("Total number of events processed = %d\n",eventNumber);

}

