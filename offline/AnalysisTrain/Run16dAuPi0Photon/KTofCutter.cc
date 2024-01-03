#include <iostream>
#include <fstream>
#include <string>

#include <TGraph.h>
#include <TFile.h>
#include <TString.h>

//#include "EmcAnaCommon.h"
#include "KTofCutter.h"
#include "TOAD.h"

// ClassImp(KTofCutter)
//
using namespace std;


KTofCutter* KTofCutter::instance = NULL;
bool KTofCutter::instanciated   = false;

KTofCutter::KTofCutter() {  /// use this constructor
  for (short i=0; i<N_ARMSECT; i++) 
    for (short j=0; j<2; j++) 
       gtof[i][j] = NULL;
}

int KTofCutter::init(int prod) { /// 0 - dAu, 1 - pp
  TString sfin;
  if(prod!=0 && prod!=1) { // sanity check 
    std::cerr <<  __PRETTY_FUNCTION__ << " " << __LINE__ << std::endl << "*** ERROR invalid production specified: " << prod << std::endl << "Use 0 for d+Au or 1 for p+p,. \t Aborting now"<< std::endl;
    return -9999;
  }
  if(prod==0) sfin="tofinterpolation.dAu.root";
  if(prod==1) sfin="tofinterpolation.pp.root";

  TOAD toad_loader("Run16dAuPi0Photon");
  string file_location1 = toad_loader.location(sfin.Data());
  TFile *fin = new TFile(file_location1.c_str(),"READ");

//  TFile *fin = new TFile(sfin.Data(),"READ");

  for (short i=0; i<N_ARMSECT; i++) 
    for (short j=0; j<2; j++) 
      fin->GetObject(Form("gtof%d%s",i,(j==0)?"m":"w"),gtof[i][j]); //  read the graphs in
  
  fin->Close();
  delete fin;
  
  return 0;
}

double  KTofCutter::evalMean(int sec, double pt) {
  if (sec<0 || sec>7) { 
    std::cerr <<  __PRETTY_FUNCTION__ << " " << __LINE__ << std::endl << "Sector is 0 to 7, do not call me with " << sec<< std::endl;
    return -9999;
  }
  return gtof[sec][0]->Eval(pt);
}

double  KTofCutter::evalWidth(int sec, double pt) {
  if (sec<0 || sec>7) { 
    std::cerr <<  __PRETTY_FUNCTION__ << " " << __LINE__ << std::endl << "Sector is 0 to 7, do not call me with " << sec<< std::endl;
    return -9999;
  }
  return gtof[sec][1]->Eval(pt);
}

double KTofCutter::tofAbsOffset(int sec, double pt, double tof) {
  if (sec<0 || sec>7) { 
    std::cerr <<  __PRETTY_FUNCTION__ << " " << __LINE__ << std::endl << "Sector is 0 to 7, do not call me with " << sec<< std::endl;
    return -9999;
  }
  return (tof - (gtof[sec][0]->Eval(pt)));
}


double KTofCutter::tofRelOffset(int sec, double pt, double tof) {
  if (sec<0 || sec>7) { 
    std::cerr <<  __PRETTY_FUNCTION__ << " " << __LINE__ << std::endl << "Sector is 0 to 7, do not call me with " << sec<< std::endl;
    return -9999;
  }
  return (tof - (gtof[sec][0]->Eval(pt)))/(gtof[sec][1]->Eval(pt));
}


KTofCutter::~KTofCutter() { /// destructor
  for (short i=0; i<N_ARMSECT; i++) 
    for (short j=0; j<2; j++) 
      if(gtof[i][j]) delete gtof[i][j];
}

