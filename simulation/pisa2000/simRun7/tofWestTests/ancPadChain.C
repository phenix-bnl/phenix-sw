void ancPadChain() {

  cout << "\n\n Recording library path for this job:" << endl;
  gSystem->Exec("echo $LD_LIBRARY_PATH");
  cout << "\n\n Recording binary path for this job:" << endl;
  gSystem->Exec("echo $PATH");
  cout << endl << endl;

  char fileName[100];
  int startRun = 90801;
  int nRuns = 500;
  int endRun = startRun + nRuns;
  int nGood = 0;
  TChain ch("AncPad2");
  for (int iRun=startRun; iRun<endRun; iRun++) {
    sprintf(fileName, "./done/ancpad2-%d-0090.rootg", iRun);
    TFile *f1 = new TFile(fileName); 
    if(!f1) {
      cerr << "\n Error in opening file " << fileName << endl;
      continue;
    } // check for opening file

    TNtuple *AncPad2 = (TNtuple*)f1->Get("AncPad2");
    if(AncPad2) {
      cout << "\n Found AncPad2 NTUPLE in file " << fileName;
      ch.Add(fileName);
      nGood++;
    }
    else {
      cout << "\n Did not find AncPad2 NTUPLE in file " << fileName << endl;
     continue;
    } // check if AncPad2 NTUPLE is found

    if(f1)
     f1->Close();

  } // loop over run numbers
  cout << "\n Number of good runs = " << nGood << endl;

  sprintf(fileName, "ancpad2Chain-%dTo%d-0090.root", startRun, endRun-1);
  ch.Merge(fileName);

}
