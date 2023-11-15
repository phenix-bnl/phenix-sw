#include <iostream>
#include "PdbErtSMMask.hh"
//=============================================================
PdbErtSMMask::PdbErtSMMask(){
  Reset();
};
//=============================================================
bool PdbErtSMMask::Reset(){
  int iarm,isector,ism,itrig;
  iarm = NARM;
  while( iarm-- ){
    isector = NSECTOR;
    while( isector-- ){
      ism = NSM;
      while( ism-- ){
	itrig = NTRIGTYP;
	while( itrig-- ){
	  Set(iarm,isector,ism,itrig,0);
	}
      }
    }
  }
  return true;
};
//=============================================================
void PdbErtSMMask::Set(int arm,int sector,int sm,int triggertype,int bit){
  fmask[arm][sector][sm][triggertype] = bit;
};
//=============================================================
int PdbErtSMMask::Get(int arm,int sector,int sm,int triggertype){
    return(fmask[arm][sector][sm][triggertype]);
};
//=============================================================
void PdbErtSMMask::printheader() const {
  std::cout<<" PdbErtSMMAsk.. "<<std::endl;
  std::cout<<" Version : "<<version<<std::endl;
  std::cout<<" First_run : "<<first_run<<std::endl;
  std::cout<<" Last_run  : "<<last_run<<std::endl;
};
//=============================================================
void PdbErtSMMask::print() const {
  std::cout<<" PdbErtSMMAsk.. "<<std::endl;
  std::cout<<" Version : "<<version<<std::endl;
  std::cout<<" First_run : "<<first_run<<std::endl;
  std::cout<<" Last_run  : "<<last_run<<std::endl;

  int iarm, isect, ism, itrig;
  iarm = NARM;
  while( iarm-- ){
    isect = NSECTOR;
    while( isect-- ){
      itrig = NTRIGTYP;
      while( itrig-- ){
        std::cout << std::endl;
	std::cout<<" ->arm:"<<iarm<<" sector:"<<isect<<" itrig:"<<itrig << " " ;
	ism = NSM;
	while( ism-- ){
	  std::cout<<" "<<fmask[iarm][isect][ism][itrig];
	  }
      }
    }
  }
  std::cout << std::endl;
};
//=============================================================
