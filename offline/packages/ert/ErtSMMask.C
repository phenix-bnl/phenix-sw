#include "ErtSMMask.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbBankID.hh"
#include "PdbCalBank.hh"
#include "PHTimeStamp.h"
#include "PdbErtSMMask.hh"

#include <cstdlib>
#include <iostream>

//====================================================
ErtSMMask::ErtSMMask(int run_number){
  if( run_number > 0 ){
    FetchByRunNumber(run_number);
  } else {
    Reset();
  }
};
//====================================================
bool ErtSMMask::FetchByRunNumber(int run_number){
  Reset();

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();  
  std::cout<<" ErtSMMask:: Opening FD in update mode.."<<std::endl;
  if (application->startRead()) {
    //PHTimeStamp tStart = PHTimeStamp(2001,1,3,0,0,0);
    PdbBankID bankID(0);
    const char *calibname = "calib.ert.smmask";
    PdbCalBank *ertBank;
    PdbErtSMMask *ertsmmask;
    ertBank = bankManager->fetchBank("PdbErtSMMaskBank",bankID,calibname,run_number);
    if( ertBank ){
      ertBank->printHeader();
      ertBank->print();
      ertsmmask = (PdbErtSMMask*)&(ertBank->getEntry(0));
      version = ertsmmask->GetVersion();
      first_run = ertsmmask->GetFirstRun();
      last_run = ertsmmask->GetLastRun();
      int iarm,isector,ism,itrig;
      //---------------------------------------------------
      iarm = NARM;
      while( iarm-- ){
	isector = NSECTOR;
	while( isector-- ){
	  ism = NSM;
	  while( ism-- ){
	    itrig = NTRIGTYP;
	    while( itrig-- ){
	      Set(iarm,isector,ism,itrig,ertsmmask->Get(iarm,isector,ism,itrig));
	    }
	  }
	}
      }
      //---------------------------------------------------
      application->abort();
    }
  } else {
    std::cout<<" ErtSMMask::Error!! failed to start application for update" << std::endl;
    exit(0);
  }
  return true;
};
//====================================================
void ErtSMMask::Reset(){
  version = -1;
  first_run = 0;
  last_run = 0;
  int iarm,isector,ism,itrig;
  for(iarm=0;iarm<NARM;iarm++){
    for(isector=0;isector<NSECTOR;isector++){
      for(ism=0;ism<NSM;ism++){
	for(itrig=0;itrig<NTRIGTYP;itrig++){
	  Set(iarm,isector,ism,itrig,0);
	} 
      }
    }
  }
};
//====================================================
void ErtSMMask::Print(){
  std::cout<<" Version : "<<version
	   <<" Run:("<<first_run<<","<<last_run<<")"<<std::endl;
  int iarm, isect, ism, itrig;
  int ismx, ismy;
  bool b_pbgl;

  iarm = NARM;
  while( iarm-- ){
    isect = NSECTOR;
    while( isect-- ){
      if( iarm == 1 && isect < 2 ) b_pbgl = true;
      else b_pbgl = 0;
      std::cout<<" "<<(iarm==1?"E":"W")<<isect;
      itrig = NTRIGTYP;
      while( itrig-- ){
	if( b_pbgl ) std::cout<<" ---------------- ";
	else std::cout<<" ------------ ";
      }
      std::cout<<std::endl;
      //---------------------
      ismy = b_pbgl ? 6 : 3;
      while( ismy-- ){
	std::cout<<"   |";
	itrig = NTRIGTYP;
	while( itrig-- ){
	  ismx = b_pbgl ? 8 : 6;
	  while( ismx-- ){
	    ism = ismx * (b_pbgl? 8 : 6 ) + ismy;
	    std::cout<<" "<<(Get(iarm,isect,ism,itrig)>0 ? "1" : "0");
	  }
	  std::cout<<" |";
	}
	std::cout<<std::endl;
      }
      //---------------------
    }
  }
  //std::cout<<" =============================================== "<<std::endl;
  
  
};
//====================================================
