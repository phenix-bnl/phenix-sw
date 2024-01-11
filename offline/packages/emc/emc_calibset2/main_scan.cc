#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <cstring>
#include <iostream.h>
#include <cmath>
#include <fstream.h>
#include <vector>
#include <TROOT.h>
#include <TApplication.h>
#include <TChain.h>
#include "clust_scan.hh"
#include "chain_udst_emc.cc"

TROOT clustscan("clustscan","TOF scannning");

main(int argc,char **argv){

  TApplication *theApp = new TApplication("App",&argc,argv);

  chain_udst();
  clust_scan();

  theApp->Run();
  delete theApp;

}
//
