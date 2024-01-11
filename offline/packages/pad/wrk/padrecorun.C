//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void padrecorun(const Int_t maxEvents=1, const char *prdfIFile="phnx.prdf", const char *parIFile="rawpar.root") {

  Int_t eventNumber = 0;

  // Executing initialization and parameter macros
  gROOT->Macro("padrecoini.C");
  gROOT->Macro("padrecopar.C");

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


      if (verbose>10) printf("Calling PadCalibration\n");
      padStatCh = PadCalibration->FetchBadChFromFile();
      padStatROC = PadCalibration->FetchBadROCFromFile();

      if (verbose>10) printf("Calling mPadDetGeo\n");
      mPadDetGeo->FetchFromFile();
      mPadDetGeo->Fetch_dPadGeom(topNode);

      if (verbose>10) printf("Calling padInclBad\n");
      padStatCh = padInclBad->FetchCalDataFromFiles();
 
    }

    if (verbose>10) printf("Calling event modules\n");
    if (verbose>10) printf("Calling PadGetDCM\n");
     PadGetDCM(topNode);

    dPadFEMPar->set_pcnumber(0,0);
    mPc1Unpack->set_pcnumber(0);
    if (verbose>10) printf("Calling mPc1Unpack\n");
     mPc1Unpack->event(topNode);

    dPadFEMPar->set_pcnumber(0,1);
    mPc2Unpack->set_pcnumber(1);
    if (verbose>10) printf("Calling mPc2Unpack\n");
     mPc2Unpack->event(topNode);

    dPadFEMPar->set_pcnumber(0,2);
    mPc3Unpack->set_pcnumber(2);
    if (verbose>10) printf("Calling mPc3Unpack\n");
     mPc3Unpack->event(topNode);

    if (verbose>10) printf("Calling padInclBad\n");
    padInclStat = padInclBad->event(topNode);
    dPadRecPar->set_pcnumber(0,0);
    mPc1Rec->set_pcnumber(0);
    if (verbose>10) printf("Calling mPc1Rec\n");
     mPc1Rec->event(topNode);

    dPadRecPar->set_pcnumber(0,1);
    mPc2Rec->set_pcnumber(1);
    if (verbose>10) printf("Calling mPc2Rec\n");
     mPc2Rec->event(topNode);

    dPadRecPar->set_pcnumber(0,2);
    mPc3Rec->set_pcnumber(2);
    if (verbose>10) printf("Calling mPc3Rec\n");
     mPc3Rec->event(topNode);


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
    if (mainIter.cd("PAD")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
  }


    if (verbose>10) printf("Calling padInclBad\n");
     padInclBad->event(topNode);
  // Take out the garbage
  delete parIn;
  if (verbose>10) printf("Total number of events processed = %d\n",eventNumber);

}

