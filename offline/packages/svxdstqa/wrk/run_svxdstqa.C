#ifndef __CINT__

#include <Fun4AllServer.h>
#include <Fun4AllNoSyncDstInputManager.h>
#include <recoConsts.h>

#include "SvxDstQA.h"
#include "SvxFillHistoPixel.h"
#include "SvxFillHistoStrip.h"

#endif

#include <TSystem.h>


#include <iostream>

using namespace std;

void run_svxdstqa(int nevt, const char *inname, const char* outname) {
  
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
   gSystem->Load("libsimreco.so");

  gSystem->Load("libsvx.so");
  //  gSystem->Load("libTHmul.so");

  gSystem->Load("libcompactCNT.so");
  //  gSystem->Load("libKalFit.so");

  gSystem->Load("libSvxDstQA.so");

  gSystem->ListLibraries();

  cout<<"Infile  "<<inname<<endl;
  cout<<"Outfile "<<outname<<endl;


  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(1);

  recoConsts *rc = recoConsts::instance();

  //  SubsysReco *svxpar = new SvxParManager();
  //  se->registerSubsystem(svxpar);

  SubsysReco *ana = new SvxDstQA(outname);
  ana->Verbosity(1);


  // add pixel
  bool res;
  SvxFillHisto *pxlhisto = new SvxFillHistoPixel();
  //res = (dynamic_cast<SvxDstQA*>ana)->registerFillHisto(pxlhisto);
  res = ((SvxDstQA*)ana)->registerFillHisto(pxlhisto);
  // add strip
  SvxFillHisto *stphisto = new SvxFillHistoStrip();
  //res = (dynamic_cast<SvxDstQA*>ana)->registerFillHisto(stphisto);
  res = ((SvxDstQA*)ana)->registerFillHisto(stphisto);


  // register
  se->registerSubsystem(ana);



  

// Input manager(s)
  Fun4AllInputManager *in1 = new Fun4AllNoSyncDstInputManager("DSTin1","DST");
  in1->Verbosity(1);
  se->registerInputManager(in1);

  cout << "Analysis started " << endl;
  gSystem->Exec("date");

  //in1->AddListFile("list.txt");
  se->fileopen("DSTin1",inname);
  //se->fileopen("DSTin1","SVX_ALL_run11_vtx_online_ana236-0000340496-0000.root");


  cout <<endl<<endl;
  cout <<"DST FILE is opened"<<endl;
  cout <<endl<<endl;

  //int max=10;
  //int max=2000;
  //int max=0;
  //se->run(0);     //process all events in the run
  se->run(nevt);     //process all events in the run

  /*
  char answer = 'y';
  while(answer != 'n') {
    se->run(1);    // process one more event
    cout <<"next?"<<endl;
    cin>>answer;
  }
  */

  if(nevt!=0){
    se->fileclose("DSTin1"); // this is necessary only if you don't process all events
  }

  se->End();

  cout << "Analysis finished " << endl;
  gSystem->Exec("date");

}


