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
#include "trk_ass.hh"

TROOT main_trk_ass("main_trk_ass","track association anallysis");

main(int argc,char **argv){

  // *** Initialization *********************
  if( argc != 3 && argc != 4 && argc != 5 && argc != 6 ){
    cout<<" Usage:: "<<argv[0]<<" inputfile outputfile (maxevent)"<<endl;
    cout<<"         "<<argv[0]<<" filelist outputfile start stop (maxevent)"<<endl;
    exit(0);
  }
  char infile[100];
  char outfile[100];
  int start,stop;
  sprintf(infile,"%s",argv[1]);
  sprintf(outfile,"%s",argv[2]);
  if( argc == 5 ){
    start = atoi( argv[3] );
    stop = atoi( argv[4] );
  }    
  int maxevent = 0;
  if( argc == 4 )
    maxevent = atoi( argv[3] );
  else if ( argc == 6 )
    maxevent = atoi( argv[5] );

  // *** Open uDST *********************
  //TApplication *theApp = new TApplication("App",&argc,argv);
  TFile* tfile;
  TTree *nt_evt,*nt_emc,*nt_trk;
  if( argc == 3 || argc == 4){
    tfile = new TFile(infile);
    nt_evt = (TTree*)tfile->Get("nt_evt");
    nt_emc = (TTree*)tfile->Get("nt_emc");
    nt_trk = (TTree*)tfile->Get("nt_trk");
  } else {
    cout<<" main_trk_ass:: Open input ntuple list "<<infile<<endl;
    nt_evt = (TTree*)new TChain("nt_evt","Event analysis chain");
    nt_trk = (TTree*)new TChain("nt_trk","Track analysis chain");
    nt_emc = (TTree*)new TChain("nt_emc","EMCal Cluster analysis chain");
    ifstream fin(infile);
    char fname[256];
    int runnum = 0;
    while( fin >> fname){
      runnum++;
      if( runnum >= start && runnum <= stop ){
	cout<<" Read and add into chain : "<<runnum<<" : "<<fname<<endl;
	((TChain*)nt_evt)->Add(fname);
	((TChain*)nt_trk)->Add(fname);
	((TChain*)nt_emc)->Add(fname);
      }
    }
  }

  // *** Create Evt class *********************
  Evt* evt =  new Evt();
  evt->Init_run1udst(nt_evt,nt_trk,nt_emc);

  // *** Process *********************
  cout<<" Process trk_ass analysis ... "<<endl;
  trk_ass(evt,outfile,maxevent);

  // *** Close uDST *********************
  //theApp->Run();
  //delete theApp;
  if( argc == 3 ){
    tfile->Close();
  } else if( argc == 5 ){
    delete nt_evt;
    delete nt_trk;
    delete nt_emc;
  }

}
//

