//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

// This macro is valid for the new library dated Aug 8 at 06:53 am.

void camresprun(const Int_t maxEvents=20, const char *pisaIFile="pisa.root", const char *prdfOFile="3bbc.prdf", const char *parOFile="3bbcpar.root", const char *relOFile="3bbcrel.root", const Int_t runNumber=0) {

  Int_t eventNumber = 0;

  TH1F *hh1 = new TH1F("hh1","",4100,0.,4100);

  // Executing initialization and parameter macros
  gROOT->Macro("camrespini.C");
  gROOT->Macro("camresppar.C");

  mainIter.cd();

  if (verbose>5) printf("Entering event loop.\n");
  while (kevent < maxEvents) {

    // Fetch a PISA99 event
    eventNumber = kevent + 1;
    pisarun->GetOneEvent(pisaevent,&kevent,T);

    mainIter.cd();

    if (verbose>5) printf("Fetched event %d\n",eventNumber);

    KinGetGEA(topNode);
    BbcGetGEA(topNode);

    printf("Calling first event only modules.\n");
    if (eventNumber == 1) {

      if (verbose>10) printf("Calling mBbcSetGeo\n");
      mBbcSetGeo->event(topNode);

      if (verbose>10) printf("Calling mBbcSetUcal\n");
      mBbcSetUcal->event(topNode);

    }

    if (verbose>10) printf("Calling event modules\n");

    if (verbose>10) printf("Calling mBbcGhitRaw\n");
     mBbcGhitRaw->event(topNode);

    cout << "After" << endl;
    for (int ipmt=0;ipmt<128;ipmt++){
      //cout << ipmt << " " << dBbcRaw->get_Adc(ipmt) << " " << dBbcRaw->get_Tdc0(ipmt) << " " << dBbcRaw->get_Tdc1(ipmt) << endl;
      hh1->Fill( (float)dBbcRaw->get_Adc(ipmt) );
    }


    if (verbose>10) printf("Calling mBbcFEM\n");
     mBbcFEM->event(topNode);

    if (verbose>10) printf("Calling mBbcDCM\n");
     mBbcDCM->event(topNode);

    if (verbose>10) printf("Calling BbcPutDCM\n");
    BbcPutDCM(topNode);

    PHBoolean prdfStatus = prdfOut->write(prdfNode);
    relOut->write(evaNode);

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

    pisarun->HitsClear();

  }

  // Take out the garbage
  parOut->write(parNode);
  delete parOut;
  delete relOut;
  delete prdfOut;
  pisaFile->Close();


}

