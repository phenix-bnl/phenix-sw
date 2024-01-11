void bbcResp(Int_t maxEvents=1, Int_t PRDFverbose=1, const char *fname="/phenix/data02/rhphemds/pisatest/PISACent10Hijing.root")
{
  //
  // Set up the PHOOL initialization 
  //
  gROOT->Macro("phoolInit.C");

  //
  // Set up the PISA interface initialization 
  //
  gROOT->Macro("pisaInit.C");
  gROOT->Macro("pisaFileOpen.C");

  //
  // Set up the Bbc initialization 
  //
  gROOT->Macro("bbctestini.C");
  gROOT->Macro("bbctestpar.C");

  //
  // BBC setup module calls
  //
  mBbcSetGeo->event(topNode);
  mBbcSetUcal->event(topNode);

  //
  // Open the output file for the GEANT Evaluation information
  //
  PHNodeIOManager *ioEval = new PHNodeIOManager("geantEval.root", PHWrite);

  //
  // Open the output file for the PRDF
  //
  PHRawOManager *ioPRDF = new PHRawOManager("PISAEvent.prdf", 0, 40*1024*1024/4);

  // gdb pause
  // int intest;
  // cout << "\n Pause " << endl;
  // cin  >> intest;
  // cout << "\n Intest =  " << intest << endl;

  PHNodeReset reset;

  Int_t kevent = 0;  // counts number of full events processed
  //
  // Read and process PISA events
  //
  while (kevent < maxEvents) {

    //
    // NOTE: kevent is incremented by the GetOneEvent method
    //
    pisarun->GetOneEvent(pisaevent, &kevent, T);  // puts hits information into subsystem classes

    mainIter.cd();

    KinGetGEA(topNode);  // fill the fkin table
    BbcGetGEA(topNode);  // fill the bbcghit table
    if(kevent <= PRDFverbose){
      bbcghit->Show();
      fkin->Show(); 
     cout << "\n";
    }

    mBbcGhitRaw->event(topNode);
    if(kevent <= PRDFverbose){
      dBbcGhitRaw->Show(); 
      dBbcRaw->Show(); 
     cout << "\n";
    }

    mBbcFEM->event(topNode);
    if(kevent <= PRDFverbose){
      dBbcFEM->Show(); 
     cout << "\n";
    }

    mBbcDCM->event(topNode);
    if(kevent <= PRDFverbose){
      dBbcDCM->Show(); 
     cout << "\n";
    }

    BbcPutDCM(topNode);        // put DCM table in PRDF node and On-Line buffers

    //
    // write out the GEANT evaluation tables
    //
    ioEval->write(evaNode);

    //
    // write out the PRDF
    //
    PHBoolean prdfStatus = ioPRDF->write(prdfNode);
    cout << "\n prdfStatus = " << prdfStatus << endl;
    ioPRDF->print();

   // Reset all data for this event (PRDF subnodes are reset internally)
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
    if (mainIter.cd("GEA")) {
      // cout << "\n In GEA " << endl;
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("EVA")) {
      // cout << "\n In EVA " << endl;
      mainIter.forEach(reset);
      mainIter.cd();
    }

    //
    // Clean up memory use at end of full event
    //
    pisarun->HitsClear();  // releases memory assigned to XxxPISAHit globals

  }  // loop over full events

  delete pisaevent;  // remove instance created with new
  delete pisarun;    // remove instance created with new

  delete ioPRDF;
  delete ioEval;

  pisaFile->Close();  // close the PISA hits file
}

