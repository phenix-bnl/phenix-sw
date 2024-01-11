//void analyze_svx()
// This macro is an example of reading root TTree
// created in svxRecoOut.root file by running svxreco.C macro.
{
  cout << "analyze_svx: Loading libraries...\n";
  gSystem->Load("libphool.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("liblvl2.so");
  gSystem->Load("libsvx.so");
  cout << "analyze_svx: Libraries loaded...\n";
  
  // Open svxRecoOut.root file and connect the TTree
  // """""""""""""""""""""""""""""""""""""""""""""""
  TFile svxDst("svxRecoOut.root");
  TTree *svxData = svxDst->Get("svx");

  SvxGhitList          *d_ghit;              // = new SvxGhitListv1();
  SvxRawhitList        *d_rawhit;            // = new SvxRawhitListv1();
  SvxGhitRawhitList    *d_ghit2rawhit;       // = new SvxGhitRawhitListv1();
  SvxClusterList       *d_cluster;           // = new SvxClusterListv1();
  SvxRawhitClusterList *d_rawhit2cluster;    // = new SvxRawhitClusterListv1();
  SvxGhitClusterList   *d_ghit2cluster;      // = new SvxGhitClusterListv1();

  TBranch *ghits          = svxData->GetBranch("svxd_ghit");
  TBranch *rawhits        = svxData->GetBranch("svxd_rawhit");
  TBranch *ghit2rawhits   = svxData->GetBranch("svxd_ghit2rawhit");
  TBranch *cluster        = svxData->GetBranch("svxd_cluster");
  TBranch *rawhit2cluster = svxData->GetBranch("svxd_rawhit2cluster");
  TBranch *ghit2cluster   = svxData->GetBranch("svxd_ghit2cluster");

  ghits->SetAddress(&d_ghit);
  rawhits->SetAddress(&d_rawhit);
  ghit2rawhits->SetAddress(&d_ghit2rawhit);
  cluster->SetAddress(&d_cluster);
  rawhit2cluster->SetAddress(&d_rawhit2cluster);
  ghit2cluster->SetAddress(&d_ghit2cluster);

  // Process events
  // """"""""""""""
  int nEnt = svxData->GetEntries();
  cout << "nEvent = " << nEnt << endl;

  //for ( int iev = 0; iev < nEnt; iev++ ) {
  for ( int iev = 0; iev < 1; iev++ ) {
    d_ghit->Reset();
    d_rawhit->Reset();
    d_ghit2rawhit->Reset();
    d_cluster->Reset();
    d_rawhit2cluster->Reset();
    d_ghit2cluster->Reset();

    svxData->GetEvent(iev);
    // ===============
    // Analysis script
    // ===============
  }
}
