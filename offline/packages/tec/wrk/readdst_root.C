void readdst_root(const char* dstFile="dstout26040.root") {

gSystem->Load("libpreco.so");

  TFile *f=new TFile(dstFile);

  TTree *T=(TTree*)gDirectory->Get("T");
  T->Print();

  dTecTrackWrapper *dTecTrack     = new dTecTrackWrapper();
  dDchTracksWrapper *dDchTracks   = new dDchTracksWrapper();
  dPadClusterWrapper *dPc3Cluster = new dPadClusterWrapper();

  T->SetBranchAddress("DST/dTecTrack", &dTecTrack);
  T->SetBranchAddress("DST/dDchTracks", &dDchTracks);
  T->SetBranchAddress("DST/dPc3Cluster", &dPc3Cluster);

//  T->SetBranchStatus("DST/dTecTrack", 1);

//  for(int j=0; j<T->GetEntries(); j++) {
  for(int j=0; j<10; j++) {

    T->GetEvent(j);

cout << dTecTrack->RowCount() << " " << dDchTracks->RowCount() << " "
     << dPc3Cluster->RowCount() << endl;

  }

}

