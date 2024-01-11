//************************************************************
// EMC Reading Rejects.list
//************************************************************
#ifdef COMPILE
#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <cstring>
#include <iostream.h>
#include <fstream.h>
#include <cmath>
#include <Rtypes.h>
#include <TSystem.h>
#include <TROOT.h>
#include <TDirectory.h>
#include <TFile.h>
#include <TKey.h>
#include <TTree.h>
#include <TBranch.h>
#include <TNtuple.h>
#include <TCanvas.h>
#include <TPad.h>
#include <TStyle.h>
#include <TPostScript.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#endif

#define GLOBAL
#include "emcRejectList.hh"

void emcRejectList(char* dir){
  cout<<" emcRejectList:: ---------------------------------------------------------- "<<endl;
  cout<<" emcRejectList:: search parameter files in "<<dir<<endl;

  int n;
  int isect,ismz,ismy,iz,iy,itower,twrnum;
  int iz_tmp,iy_tmp;
  int irun,runnum;
  isect = 8;
  // --- Initialization -------------------
  while( isect -- >0){
    iz = 96;
    while( iz-- >0){
      iy = 48;
      while( iy-- >0 ){
	frejtwr[isect][iz][iy] = 0;
      }
    }
  }
  irun = 100000;
  while( irun-- )
    frunstat[irun] = 0;
  //--------------------------------------
  // --- Tower QA
  twrnum = 0;
  char cx[8],cy[8],tmpc;
  char fname[128];
  sprintf(fname,"%s/%s",dir,"emcRejectList.dat");
  ifstream fin(fname);  
  cout<<" emcRejectList:: Reading "<<fname<<endl;
  while( fin>>n>>itower>>cx>>iz>>cy>>iy ){
    while(fin.get(tmpc)&&tmpc!='\n');
    //
    // The definition of iz_tmp,iy_tmp were inverted...
    //  .. found 011028...
    //
    if( itower < 15552 ) {
      isect = (int)(itower/2592);
      iz_tmp = itower%72;
      iy_tmp = (int)((itower - isect*2592)/72);
    } else {
      isect = (int)((itower - 15552)/4608);
      iz_tmp = (int) ( (itower - 15552 - isect*4608 )%96 );
      iy_tmp = (int) ( (itower - 15552 - isect*4608 )/96 );
      isect = isect + 6;
    }
    if( iz != iz_tmp || iy != iy_tmp ){
      cerr<<" Reject.lists has wrong number sets of z,y"<<endl;
      cout<<"                 (iz,iy) = ("<<iz<<","<<iy<<") "<<endl;
      cout<<"                 (iz,iy) from towerid = ("<<iz_tmp<<","<<iy_tmp<<") "<<endl;
      cout<<"                  isect = "<<isect<<endl;
      fin.close();
      exit(0);
    }
    frejtwr[isect][iz][iy] = 1024;
    twrnum ++ ;
  }
  if( twrnum == 0 ){ cout<<" emcRejectList:: Warning!!! No entries in emcRejectList.dat. "<<endl;  }
  cout<<" emcRejectList:: sick "<<twrnum<<" tower "<<endl;
  fin.close();

  //--------------------- Edge 
  isect = 8;
  while( isect-- ){
    iz = (isect>=6 ? 96 : 72 );
    while( iz-- ){
      iy = (isect>=6 ? 48 : 36 );
      while( iy-- >0 ){
	if( (isect >=6 && (iz < 2 || iz >= 94 ) ) ||
	    (isect >=6 && (iy < 2 || iy >= 46 ) ) ||
	    (isect < 6 && (iz < 2 || iz >= 70 ) )  ||
	    (isect < 6 && (iy < 2 || iy >= 34 ) ) )
	  frejtwr[isect][iz][iy] += 512;
      }
    }
  }
  cout<<" emcRejectList:: remove the edge 2 towers!!"<<endl;

  //--------------------- PbGl
  sprintf(fname,"%s/%s",dir,"emcRejectList_PbGl.dat");
  ifstream ffin(fname);
  cout<<" emcRejectList:: Reading "<<fname<<endl;
  twrnum = 0;
  while( ffin>>n>>itower ){
    int itmp = itower % 15552;
    isect = (int)(itmp/4608) + 6;
    iy = (int)(( itmp % 4608 ) / 96 );
    iz = (int)(( itmp % 4608 ) % 96 );
    frejtwr[isect][iz][iy] = 1024;
    twrnum ++ ;
  }
  if( twrnum == 0 ){ cout<<" emcRejectList:: Warning!!! No entries in emcRejectList_PbGl.dat "<<endl;  }
  cout<<" emcRejectList:: emcRejectList_PbGl.dat with dead "<<twrnum<<" tower "<<endl;
  ffin.close();

  // -------------------- Histgrams
  int nqa = 0;
  int nqa_sect[8];
  char hname[64],htitle[64];
  isect = 8;
  while( isect-- ){
    nqa_sect[isect] = 0;
    sprintf(hname,"hRejsect_%d",isect);
    sprintf(htitle,"Reject list sector %d",isect);
    if( TObject* obj = gDirectory->Get(hname) ) delete obj;
    if( isect >=6 )
      hRejsect[isect] =  new TH2F(hname,htitle,96,0,96,48,0,48);
    else
      hRejsect[isect] =  new TH2F(hname,htitle,72,0,72,36,0,36);
    iz = (isect>=6 ? 96 : 72 );
    while( iz-- ){
      iy = (isect>=6 ? 48 : 36 );
      while( iy-- ){
	hRejsect[isect]->Fill(iz,iy,frejtwr[isect][iz][iy]);
	if( frejtwr[isect][iz][iy] > 0 ){
	  nqa++;
	  nqa_sect[isect]++;
	}
      }
    }
    cout<<"                          QA sector "<<isect<<" is "<<nqa_sect[isect]<<endl;
  }
  cout<<" emcRejectList:: TH2F *hRejsect[8] is created. Total QA tower "<<nqa<<endl;

  //--------------------- Run QA
  sprintf(fname,"%s/%s",dir,"emcRejectList_run.dat");
  ifstream fin_run(fname);  
  cout<<" emcRejectList:: Reading "<<fname<<endl;
  runnum = 0;
  while( fin_run>>n>>irun ){
    while(fin_run.get(tmpc)&&tmpc!='\n');
    frunstat[irun] = 1024;
    runnum ++ ;
  }
  if( runnum == 0 ){ cout<<" emcRejectList:: Warning!!! No entries in emcRejectList_run.dat "<<endl;  }
  cout<<" emcRejectList:: MEMO:: For run1, emcRejectList_run.dat was HEALTHY run list. "<<endl;
  cout<<" emcRejectList:: MEMO:: For run2, emcRejectList_run.dat will is SICK run list. "<<endl;
  cout<<" emcRejectList:: read "<<runnum<<" run "<<endl;
  fin_run.close();

  cout<<" emcRejectList:: ---------------------------------------------------------- "<<endl;

  return;

}

