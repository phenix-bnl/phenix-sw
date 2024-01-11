#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <cstring>
#include <iostream.h>
#include <fstream.h>
#include <TROOT.h>
#include <TApplication.h>
#include <TChain.h>
#include <TFile.h>
#include "Clust.hh"
#include "TNamedDir.hh"

main(int argc,char **argv){

  // fake main function.

  cout<<" Clust "<<endl;
  Clust* c = new Clust;
  cout<<" c->arm = "<<c->arm<<endl;
  delete c;

  cout<<" TNamedDir "<<endl;
  TNamedDir* dir = new TNamedDir();
  delete dir;
}

