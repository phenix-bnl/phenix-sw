void ancChain(const int startRun=90801, const int nRuns=500) {

  //
  // ROOT macro to chain a set of anctfw2*rootg files
  //

  cout << "\n\n Recording library path for this job:" << endl;
  gSystem->Exec("echo $LD_LIBRARY_PATH");
  cout << "\n\n Recording binary path for this job:" << endl;
  gSystem->Exec("echo $PATH");
  cout << endl << endl;

  char fileName[100];
  int endRun = startRun + nRuns;
  int nGood = 0;
  TChain ch("AncTfw2");
  for (int iRun=startRun; iRun<endRun; iRun++) {
    sprintf(fileName, "./done/anctfw2-%d-0090.rootg", iRun);
    TFile *f1 = new TFile(fileName); 
    if(!f1) {
      cerr << "\n Error in opening file " << fileName << endl;
      continue;
    } // check for opening file

    TNtuple *AncTfw2 = (TNtuple*)f1->Get("AncTfw2");
    if(AncTfw2) {
      cout << "\n Found AncTfw2 NTUPLE in file " << fileName;
      ch.Add(fileName);
      nGood++;
    }
    else {
      cout << "\n Did not find AncTfw2 NTUPLE in file " << fileName << endl;
     continue;
    } // check if AncTfw2 NTUPLE is found

    if(f1)
     f1->Close();

  } // loop over run numbers
  cout << "\n Number of good runs = " << nGood << endl;

  sprintf(fileName, "anctfw2Chain-%dTo%d-0090.root", startRun, endRun-1);
  ch.Merge(fileName);

}
