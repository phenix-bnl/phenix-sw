void dstlistgenerator(char *dir,char *fin,char *fout="dstlist.txt")
{
#include "fstream.h"

  char fname[300];
  sprintf(fname,"%s/%s",dir,fin);

  TFile *dst = new TFile(fname);
  TTree* T = (TTree*) dst->Get("T");  
  int nentry = T->GetEntries(); 
  ofstream out(fout,ios::app);
  out << fin << "  " << nentry << endl;
  out.close();
}
