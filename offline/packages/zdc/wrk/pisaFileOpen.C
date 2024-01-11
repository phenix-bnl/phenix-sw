{
  //
  //  Open the input file (this code should be moved to an event iterator method)
  //
  TFile *pisaFile = new TFile(fname);
  TTree *T = (TTree*)pisaFile.Get("T");

  PISARun *pisarun = new PISARun();  // create a run control instance

  PISAEvent *pisaevent = new PISAEvent();  // create a PISAEvent instance

  TBranch *branch  = T->GetBranch("pisaevent");   
  branch->SetAddress(&pisaevent);

  Int_t nevent = T->GetEntries();

  if(maxEvents == -1){
    //
    // Assume user wants all the events in the file
    //
    maxEvents = nevent;
  }

  if(maxEvents > nevent){
    cerr << "\n You are requesting " << maxEvents << " events but";
    cerr << " the input file has only " << nevent << " events" << endl;
    cerr << " This macro is exiting " << endl;
    exit(1);
    return;
  }  // check on number of events requested

  //
  // All of above code should be moved to an event iterator method
  // This method will receive maxEvents as a modifiable input parameter
  //
}


