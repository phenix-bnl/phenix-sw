//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void padresprun(const Int_t maxEvents=1, const char *pisaIFile="pisaVRDC.root", const char *prdfOFile="phnx.prdf", const char *parOFile="rawpar.root", const char *relOFile="rawrel.root", const Int_t runNumber=0) {

  Int_t eventNumber = 0;

  // Executing initialization and parameter macros
  gROOT->Macro("padrespini.C");
  gROOT->Macro("padresppar.C");

  mainIter.cd();

  if (verbose>5) printf("Entering event loop.\n");
  while (kevent < maxEvents) {

    // Fetch a PISA99 event
    eventNumber = kevent + 1;
    pisarun->GetOneEvent(pisaevent,&kevent,T);

    mainIter.cd();

    if (verbose>5) printf("Fetched event %d\n",eventNumber);

    KinGetGEA(topNode);
    PadGetGEA(topNode);

    printf("Calling first event only modules.\n");
    if (eventNumber == 1) {

 
    }

    if (verbose>10) printf("Calling event modules\n");
    dPadSlowSimPar->set_pcnumber(0,0);
    mPc1SlowSim->set_pcnumber(0);
    if (verbose>10) printf("Calling mPc1SlowSim\n");
     mPc1SlowSim->event(topNode);

    dPadSlowSimPar->set_pcnumber(0,1);
    mPc2SlowSim->set_pcnumber(1);
    if (verbose>10) printf("Calling mPc2SlowSim\n");
     mPc2SlowSim->event(topNode);

    dPadSlowSimPar->set_pcnumber(0,2);
    mPc3SlowSim->set_pcnumber(2);
    if (verbose>10) printf("Calling mPc3SlowSim\n");
     mPc3SlowSim->event(topNode);

    dPadFEMPar->set_pcnumber(0,0);
    mPc1FEM->set_pcnumber(0);
    if (verbose>10) printf("Calling mPc1FEM\n");
     mPc1FEM->event(topNode);

    dPadFEMPar->set_pcnumber(0,1);
    mPc2FEM->set_pcnumber(1);
    if (verbose>10) printf("Calling mPc2FEM\n");
     mPc2FEM->event(topNode);

    dPadFEMPar->set_pcnumber(0,2);
    mPc3FEM->set_pcnumber(2);
    if (verbose>10) printf("Calling mPc3FEM\n");
     mPc3FEM->event(topNode);

    mPc1DCM->set_pcnumber(0);
    if (verbose>10) printf("Calling mPc1DCM\n");
     mPc1DCM->event(topNode);

    mPc2DCM->set_pcnumber(1);
    if (verbose>10) printf("Calling mPc2DCM\n");
     mPc2DCM->event(topNode);

    mPc3DCM->set_pcnumber(2);
    if (verbose>10) printf("Calling mPc3DCM\n");
     mPc3DCM->event(topNode);

    PadPutDCM(topNode,0);
    PadPutDCM(topNode,1);
    PadPutDCM(topNode,2);

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
    if (mainIter.cd("PAD")) {
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
  if (verbose>10) printf("Total number of events processed = %d\n",eventNumber);
  pisaFile->Close();


}

