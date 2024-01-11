void bbcTest(Int_t maxEvents=1, Int_t PRDFverbose=1, const char *fname="phnx.prdf")
{
  //
  // Development version of bbcTest macro
  // See README document for more details
  //

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

  // gdb pause
  // int intest;
  // cout << "\n Pause " << endl;
  // cin  >> intest;
  // cout << "\n Intest =  " << intest << endl;

  PHNodeReset reset;

  //
  // BBC setup module calls
  //
  mBbcSetGeo->event(topNode);
  mBbcSetUcal->event(topNode);

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
    BbcGetGEA(topNode);  // fill the ghit table (the name used by the Bbc in STAF)
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

    mBbcUnpack->event(topNode);
    if(kevent <= PRDFverbose){
     dBbcRaw->Show(); 
     cout << "\n";
    }

    mBbcRawOut->event(topNode);
    if(kevent <= PRDFverbose){
      dBbcOut->Show(); 
      cout << "\n";
    }

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

  pisaFile->Close();  // close the PISA hits file
}

