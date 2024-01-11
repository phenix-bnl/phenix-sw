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
//#include "trk_ass_trig.hh"
//TROOT trk_ass_trig_run("trk_ass_trig_run","track association anallysis");

trk_ass_trig_run(char* infile, char* outfile){

  gSystem->Load("libemc_dstrun.so");
  gSystem->Load("libemc_clusttr.so");
  gSystem->Load("libemc_calibset2.so");

  // *** Open DST *********************
  TFile* tfile = new TFile(infile);
  TTree* nt = (TTree*)tfile->Get("T");

  // *** Process *********************
  cout<<" Process trk_ass_trig analysis ... "<<endl;
  trk_ass_trig(nt,outfile);

  // *** Close uDST *********************
  tfile->Close();

}
//

