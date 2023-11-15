void countPISAEvent() {

  //
  // Author: C.F. Maguire
  // Creation date: February 19, 2007
  //
  // ROOT macro to count number of events in a set of PISAEvent.root files
  //

  gSystem->Load("libsimreco.so");
  ofstream fout("goodNames.txt");
  char fileName[100];
  int startRun = 90801;
  int nRuns = 500;
  int endRun = startRun + nRuns;
  int nEvents = 0;
  cout << endl;
  for (int iRun=startRun; iRun<endRun; iRun++) {
    sprintf(fileName, "./done/PISA2000_LAMDAPLPL-00000%d-0090.rootg", iRun);
    TFile *f1 = new TFile(fileName);
    if(!f1) {
      cerr << "\n Unable to find PISAEvent.root file " << fileName << endl;
      continue;
    }
    int kEvents = 0;
    TTree *T = (TTree*)f1->Get("T");
    if(T) {
      kEvents = T->GetEntries();
      nEvents += kEvents;
    }
    f1->Close();
    if(kEvents != 10000)
      cout << "\n fileName " << fileName << " has events " << kEvents;

    if(kEvents > 0)
      fout << fileName << endl;
  }
  cout << "\n nEvents " << nEvents;
  cout << endl;
  fout.close();
}
