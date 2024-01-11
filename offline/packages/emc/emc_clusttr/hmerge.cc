
#include "hmerge.hh"

int hmerge(char* filename,char* outname){

  //============================ Delete file
  TFile* ftmp = new TFile(outname);
  if( ftmp ){
    ftmp->Close();
    cout<<" hmerge:: Delete file "<<outname<<"  y[n]?: ";
    char com[256] = "y";
    cin>>com;
    char* opt_del;
    opt_del = strstr(com,"y");
    if( opt_del ){
      char command[128];
      sprintf(command,"rm %s",outname);
      gSystem->Exec(command);
    }
  }
  //============================ Merge.
  ifstream fin(filename);
  char fname[256];
  int num = 0;
  while( fin >> fname){
    cout<<" Add # "<<num<<" ("<<fname<<")  to "<<outname<<endl;
    TFile* fin1 = new TFile(fname);
    TFile* fin2 = new TFile(outname,"UPDATE");
    TFile* fout = fin2;
    int addnum = AddRecursive((TDirectory*)fin1,(TDirectory*)fin2,(TDirectory*)fout);
    cout<<". Add histgrams : "<<addnum<<endl;
    num++;
    fout->Write();
    fin1->Close();
    fout->Close();
  }
  cout<<" hmerge:: Finish to add hist file "<<num<<endl;

  return num;
};
//------------------------------------------------
