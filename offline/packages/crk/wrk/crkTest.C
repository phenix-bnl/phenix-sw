void crkTest(Int_t maxEvents=10, Int_t PRDFverbose=10, const char *fname="pisa99.root")
{
  //
  // Development version of crkTest macro
  // See README document for more details
  //

  //
  // Set up the PHOOL initialization 
  //
  gROOT->Macro("phoolInit.C");
  char ctemp;
  cout << "phoolInit.C done."<<endl;
  //
  // Set up the PISA interface initialization
  //
  gROOT->Macro("pisaInit.C");
  gROOT->Macro("pisaFileOpen.C");

  cout << "pisaInit.C and pisaFileOpen.C done."<<endl;
  //
  // Set up the crk initialization
  //
  // initialization for Dch
  gROOT->Macro("dchtestini.C");
  gROOT->Macro("dchtestpar.C");

  gROOT->Macro("crktestini.C");
  gROOT->Macro("crktestpar.C");

  //
  // Open the output file for the GEANT Evaluation information
  //
  PHNodeIOManager *ioEval = new PHNodeIOManager("geantEval.root", PHWrite);
   
  PHNodeReset reset;
    
  //
  // CRK setup module calls
  //
  mCrkSetGeo->event(topNode);
  mCrkSetUcal->event(topNode);
  mCrkSetCal->event(topNode);

  // gdb pause
  // int intest;
  // cout << "\n Pause " << endl;
  // cin  >> intest;
  // cout << "\n Intest =  " << intest << endl;

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
    CrkGetGEA(topNode);  // fill the crkghit table(STAF used ghit as table name)
   
    if(kevent <= PRDFverbose){
      fkin->Show();
      crkghit->Show();   
      cout << "\n";
    }

    mCrkGhitRaw->event(topNode);
    if(kevent <= PRDFverbose){
      dCrkUcal->Show();
      dCrkRaw->Show();
      dCrkRel2s->Show();
      cout << "\n";
    }
    
    mCrkRawFEM->event(topNode);
    if(kevent <= PRDFverbose){
      dCrkFEM->Show();
      cout << "\n";
    }

    mCrkDCM->event(topNode);
    if(kevent <= PRDFverbose){
      dCrkDCM->Show();
      cout << "\n";
    }

//
// Calling the "analysis" chain"  added by YA
//
    TCrkModule::browse(topNode, "dCrkRaw");

    cout << "now call DCMRaw"<<endl;
    mCrkDCMRaw->event(topNode);

    cout << "now call RawHit"<<endl;
    mCrkRawHit->event(topNode);
//
// track/RICH association.
// This is just to test if the old DchProj works or not
// First, lets reconstruct Dch track
    cout << "Do tracking (DC)"<<endl;
    DchGetGEA(topNode);
    dcghit->Show();

    mDchFastSim->event(topNode);
    dDchHit->Show();
    dDchRaw->Show();

    mDchTracker->event(topNode);
    dDchTracks->Show();
//
// Now try track/rich association
    cout << "before calling PID"<<endl;
    dCrkPid->Show();
    //    mCrkDchProj->event(topNode);
    mCrkProjPid->event(topNode);
    cout << "after calling PID"<<endl;
    TCrkModule::browse(topNode,"dCrkPid");
    topNode->print();
//

    cout << "reset all data in this event"<<endl;
    cout << " type to continue"<<endl;
    cin >> ctemp;
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
    if (mainIter.cd("CRK")) {
      // cout << "\n In CRK " << endl;
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("DCH")) {
      // cout << "\n In DCH " << endl;
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
