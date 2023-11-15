#include <iostream>
#include "PdbErtSMEff.hh"
//=============================================================
PdbErtSMEff::PdbErtSMEff(){
  Reset();
};
//=============================================================
bool PdbErtSMEff::Reset(){
  int iarm,isector,ism,itrig;
  iarm = NARM;
  while( iarm-- ){
    isector = NSECTOR;
    while( isector-- ){
      ism = NSM;
      while( ism-- ){
	itrig = NTRIGTYP;
	while( itrig-- ){
	  Set(iarm,isector,ism,itrig,1);
	}
      }
    }
  }
  return true;
};
//=============================================================
void PdbErtSMEff::Set(int arm,int sector,int sm,int triggertype,float eff){
  feff[arm][sector][sm][triggertype] = eff;
};
//=============================================================
float PdbErtSMEff::Get(int arm,int sector,int sm,int triggertype){
    return(feff[arm][sector][sm][triggertype]);
};
//=============================================================
void PdbErtSMEff::printheader() const {
  std::cout<<" PdbErtSMEff.. "<<std::endl;
  std::cout<<" Version : "<<version<<std::endl;
  std::cout<<" First_run : "<<first_run<<std::endl;
  std::cout<<" Last_run  : "<<last_run<<std::endl;
};
//=============================================================
void PdbErtSMEff::print() const {
  std::cout<<" PdbErtSMEff.. "<<std::endl;
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
	std::cout<<" ->arm:"<<iarm<<" sector:"<<isect<<" itrig:"<<itrig;
	ism = NSM;
	while( ism-- ){
	  std::cout<<" "<<feff[iarm][isect][ism][itrig];
	  }
      }
    }
  }
};
//=============================================================
