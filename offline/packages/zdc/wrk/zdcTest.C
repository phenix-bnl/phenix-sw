//void zdcTest(Int_t maxEvents=1, Int_t PRDFverbose=1, const char *fname="/phenix/data02/ohnishi/pisa99/bin/PISAEvent.root")
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
  gROOT->Macro("zdctestini.C");

  PHNodeReset reset;

  mZdcEvent->setEventNumber(0); 
  mZdcEvent->Clear(); 
  mZdcEvent->SetUcal(topNode);

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
    fkin->Show();
    header->Show();
    cout << "\n";
    cout << " b = " << header->get_b(0) << endl;

    mZdcEvent->Clear(); 
    mZdcEvent->setEventNumber(kevent); 

    mZdcEvent->GhitRaw(topNode);
    mZdcEvent->RawToFem(topNode);
    mZdcEvent->FemToDcm(topNode);
    mZdcEvent->DcmToRaw(topNode);


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
    if (mainIter.cd("ZDC")) {
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

    //
    // Clean up memory use at end of full event
    //
    pisarun->HitsClear();  // releases memory assigned to XxxPISAHit globals

  }  // loop over full events

  delete pisaevent;  // remove instance created with new
  delete pisarun;    // remove instance created with new

  pisaFile->Close();  // close the PISA hits file
}

