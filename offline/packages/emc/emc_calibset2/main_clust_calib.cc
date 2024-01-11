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
#include "clust_calib.hh"
#include "chain_udst.cc"

TROOT clustcalib("clustcalib","Calibration");

main(int argc,char **argv){

  TApplication *theApp = new TApplication("App",&argc,argv);

  chain_udst();
  // --- Input Ntuple
  TTree* nt_evt;
  TTree* nt_trk;
  TTree* nt_emc;
  nt_evt = (TTree*)(gROOT->FindObject("nt_evt"));
  //nt_trk = (TTree*)(gROOT->FindObject("nt_trk"));
  nt_trk = 0;
  nt_emc = (TTree*)(gROOT->FindObject("nt_emc"));
  if( nt_evt == 0 ){
    cerr<<" Error:: Can't fetch nt_evt ..."<<endl;
    exit(0);
  }
  Evt* evt =  new Evt();
  evt->Init_run1udst(nt_evt,nt_trk,nt_emc);

  // --- main program
  clust_calib(evt,"test");

  theApp->Run();
  delete theApp;

}
//
