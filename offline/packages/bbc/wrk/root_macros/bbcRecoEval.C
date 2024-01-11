void bbcRecoEval(Int_t maxEvents=1, Int_t verbose=1, const char *fname="/phenix/data02/rhphemds/pisatest/PISACent10Hijing.root") {

  Int_t eventNumber = 0;

  //
  // Set up the PHOOL initialization
  //
  gROOT->Macro("phoolRecoInit.C");

  //
  // Set up the PISA interface initialization
  //
  gROOT->Macro("pisaRecoInit.C");

  // Executing initialization and parameter macros
  gROOT->Macro("bbcevalini.C");
  gROOT->Macro("bbcevalpar.C");

  //
  // BBC setup module calls
  //
  mBbcSetGeo->event(topNode);

  // Set up input PRDF
  PHString inputFile = "phnx.prdf";

  // Set up the main iterator and event
  mainIter.cd();
  Event *thisEvent = 0;
  mainIter.addNode(new PHDataNode<Event>(thisEvent, "PRDF"));
  PHNodeReset reset;
  Eventiterator *eventIter = new fileEventiterator(inputFile.getString());
  
  Int_t kevent = 0;  // counts number of PISA events processed

  //
  // Open the PISA Input File
  //
  gROOT->Macro("pisaFileOpen.C");

  //
  // Open the GEANT Evaluation File
  //
  PHNodeIOManager *ioEval = new PHNodeIOManager("geantEval.root", PHReadOnly);

  //
  // Open the output DST file
  //
  PHNodeIOManager *ioDST = new PHNodeIOManager("DSTEval.root", PHWrite);

  gROOT->cd();

  while ((thisEvent = eventIter->getNextEvent()) && eventNumber++ < maxEvents) {

    if(eventNumber<=verbose) {
      cout << "\n Fetched event " << eventNumber << endl;
    }

    ioEval->read(evaNode);

    //
    // NOTE: kevent is incremented by the GetOneEvent method
    //
    pisarun->GetOneEvent(pisaevent, &kevent, T);  // puts hits information into subsystem classes
    KinGetGEA(topNode);  // fill the fkin table
    BbcGetGEA(topNode);  // fill the bbcghit table
      
    // Point the data node to the new event
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);


    BbcGetDCM(topNode);
    if(eventNumber<=verbose) {
      dBbcDCM->Show();
    }

    mBbcUnpack->event(topNode);
    if(eventNumber<=verbose) {
      dBbcRaw->Show();
    }

    mBbcRawOut->event(topNode);
    if(eventNumber<=verbose){
      dBbcOut->Show(); 
      cout << "\n";
    }
 
     mBbcOutEval->event(topNode);

    //
    // write out the DST
    //
    ioDST->write(dstNode);

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
    if (mainIter.cd("EVA")) {
      // cout << "\n In EVA " << endl;
      mainIter.forEach(reset);
      mainIter.cd();
    }

    //
    // Clean up PISA memory use at end of full event
    //
    pisarun->HitsClear();  // releases memory assigned to XxxPISAHit globals

  } // loop over events

  delete ioDST;       // this actually closes the output DST file

  delete pisaevent;   // remove instance created with new
  delete pisarun;     // remove instance created with new
  pisaFile->Close();  // close the PISA hits file

  delete ioEval;      // close GEANT Evaluation File

}









