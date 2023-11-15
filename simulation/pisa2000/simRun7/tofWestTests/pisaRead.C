void pisaRead() {

  cout << "\n\n Recording library path for this job:" << endl;
  gSystem->Exec("echo $LD_LIBRARY_PATH");
  cout << "\n\n Recording binary path for this job:" << endl;
  gSystem->Exec("echo $PATH");
  cout << endl << endl;

  gSystem->Load("libsimreco.so");
  int nEvents = 0;
  TFile *f1 = new TFile("PISAEvent.root"); 
  if(!f1) {
    cerr << "\n Unable to find PISAEvent.root file " << endl
    gSystem->Exec("touch anctfw.root");
    gSystem->Exec("touch anctfw2.root");
    gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");
    return;
  }
  TTree *T = (TTree*)f1->Get("T");
  if(T) {
    nEvents = T->GetEntries();
    gSystem->Exec("pisaRootRead >& read.out");
  }
  else {
    gSystem->Exec("touch anctfw.root");
    gSystem->Exec("touch anctfw2.root");
    gSystem->Exec("touch ancpad.root");
    gSystem->Exec("touch ancpad2.root");
    cerr << "\n Unable to find TTree T " << endl;
  }
  if(f1)
   f1->Close();

  if(nEvents != 10000)
    cout << "\n PISAEvent.root has fewer than expected events = " << nEvents;
  else 
    cout << "\n PISAEvent.root has expected events = " << nEvents;

  if(nEvents == 0) {
    gSystem->Exec("touch anctfw.root");
    gSystem->Exec("touch anctfw2.root");
    gSystem->Exec("touch ancpad.root");
    gSystem->Exec("touch ancpad2.root");
    gSystem->Exec("rm anctfw*root");
    gSystem->Exec("rm ancpad*root");
    gSystem->Exec("touch anctfw.root");
    gSystem->Exec("touch anctfw2.root");
    gSystem->Exec("touch ancpad.root");
    gSystem->Exec("touch ancpad2.root");
  }

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

  cout << endl;
}
