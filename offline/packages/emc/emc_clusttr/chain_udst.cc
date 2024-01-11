//#include <string>

void chain_udst(char* filename = "chain_udst.txt"){

  ifstream fin(filename);
  char fname[128];

  TChain* nt_evt = new TChain("nt_evt","Event uDST chain");
  TChain* nt_emc = new TChain("nt_emc","EMCal uDST chain");
  TChain* nt_trk = new TChain("nt_trk","EMCal uDST chain");

  int runnum = 0;
 
  while( fin>> fname ){
    runnum++;
    cout<<" Read and add into chain No."<<runnum<<" : "<<fname<<endl;
    nt_trk->Add(fname);
  }

  cout<<" TChain nt_trk was created!.."<<endl;
}
