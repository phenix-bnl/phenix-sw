#include <strstream>

TH1F *h[47];
int nhist = 47;


char *hlist[47] = {
  "sADC320","sADC336","sADC384","sADC400","sADC416",
  "sADC432","sADC448","sADC480","sADC496","sADC560",
  "sADC576","sADC592","sADC608","sADC624","sADC640",
  "sADC960","sADC976","sADC1024","sADC1072","sADC1088",
  "sADC1104","sADC1120","sADC1136","sADC1152","sADC1200",
  "sADC1232","sADC1248","sADC1264","sADC1265","sADC1584",
  "sADC1600","sADC1616","sADC1664","sADC1840","sADC1888",
  "sADC1904","sADC1936","sADC2128","sADC2208","sADC2224",
  "sADC2240","sADC2496","sADC2528","sADC2544","sADC2545",
  "sADC3520","sADC3824"
};



merge_all(const char *ofname="merge_all.root") {
  void add_h1 (char*);
  TFile *outfile = TFile::Open (ofname, "recreate", "no_field", 1);

  int i;
  for(i=0;i<nhist;i++) {
    h[i] = new TH1F (hlist[i],hlist[i],256,-100.,1024.);
    //    cout << sname.str() << "is created" <<endl;
  }

  add_h1("merge9591_11291.root");
  add_h1("merge11303_12127.root");
  add_h1("merge12134_12397.root");
  add_h1("merge12399_12468.root");

  outfile->Write();
  outfile->Close();
  delete outfile;
  
}


void add_h1(char *fname) {
  tf = new TFile(fname);
  int i;
  for(i=0;i<nhist;i++) {
    h[i]->Add((TH1F*)tf->Get(hlist[i]));
  }
  cout << "file "<<fname<<" is added" << endl;
  delete tf;
}
