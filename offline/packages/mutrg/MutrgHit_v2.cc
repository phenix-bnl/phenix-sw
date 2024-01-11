#include "MutrgHit_v2.hh"
#include "MutrgKey.hh"

#include <stdlib.h>
#include <sstream>
#include <iomanip>

using namespace std;

ClassImp(MutrgHit_v2);

//////////////////////////////////////////////////////////////

MutrgHit_v2::MutrgHit_v2(void){
  Reset();
}

//////////////////////////////////////////////////////////////

void MutrgHit_v2::Reset(void){
  location=0;
  hit_clock=0;
  state=0; // [1:mask_flag,0:dead_flag]
  return;
}

///////////////////////////////////////////////////////////////

void MutrgHit_v2::Clear(Option_t*) {
  Reset();
}

///////////////////////////////////////////////////////////////

void MutrgHit_v2::SetLocation(int arm,int station,int octant,
			      int halfoctant,int gap,int cathode,
			      int strip){
  location=MutrgKey::LocToKey(arm,station,octant,halfoctant,gap,cathode,strip);
  return;
}

///////////////////////////////////////////////////////////////////

void MutrgHit_v2::SetLocation(int arm,int station,int octant,
			      int halfoctant,int strip){
  location=MutrgKey::LocToKey(arm,station,octant,halfoctant,strip);
  return;
}

///////////////////////////////////////////////////////////////////

void MutrgHit_v2::SetHitClock(unsigned short clk,bool hit){
  if(hit){hit_clock|= (0x1<<clk);}
  else   {hit_clock&=~(0x1<<clk);}
  return;
}

//////////////////////////////////////////////////////////////////

void MutrgHit_v2::SetDeadFlag(bool flag){
  state&=(((unsigned short)flag)<<0);
  return;
}

///////////////////////////////////////////////////////////////////

void MutrgHit_v2::SetMaskFlag(bool flag){
  state&=(((unsigned short)flag)<<1);
  return;
}

///////////////////////////////////////////////////////////////////

int MutrgHit_v2::SetMutHitValue(int gap,float q,float qerr,float ptime){
  // This function is unavailable in MutrgHit_v2
  return -1;
}

///////////////////////////////////////////////////////////////////

void MutrgHit_v2::SetMutMaxCharge(float q){
  // This function is unavailable in MutrgHit_v2
  return;
}

////////////////////////////////////////////////////////////////////

void MutrgHit_v2::GetLocation(int &arm,int &station,int &octant,
			      int &halfoctant,int &gap,int &cathode,
			      int &strip) const{
  MutrgKey::KeyToLoc(location,arm,station,octant,halfoctant,gap,cathode,strip);
  return;
}

////////////////////////////////////////////////////////////////////

void MutrgHit_v2::GetLocation(int &arm,int &station,int &octant,
			      int &halfoctant,int &strip) const{
  int gap,cathode; // discard gap and cathode
  GetLocation(arm,station,octant,halfoctant,gap,cathode,strip);
  return;
}

////////////////////////////////////////////////////////////////////

bool MutrgHit_v2::GetHitClock(unsigned short clk) const{
  return (bool)(hit_clock&(0x1<<clk));
}

///////////////////////////////////////////////////////////////////

bool MutrgHit_v2::GetDeadFlag(void) const{
  return (bool)((state>>0)&0x1);
}

//////////////////////////////////////////////////////////////////

bool MutrgHit_v2::GetMaskFlag(void) const{
  return (bool)((state>>1)&0x1);
}

//////////////////////////////////////////////////////////////////

int MutrgHit_v2::GetMutHitValue(int gap,float &q,float &qerr,
				float &ptime) const{
  // This function is unavailable in MutrgHit_v2
  q=0.0;
  qerr=0.0;
  ptime=0.0;
  return -1;
}

//////////////////////////////////////////////////////////////////

float MutrgHit_v2::GetMutMaxCharge(void) const{
  // This function is unavailable in MutrgHit_v2
  return -9999.0;
}

//////////////////////////////////////////////////////////////////////

void MutrgHit_v2::ExtendHitClock(int nclk){
  if(nclk>0){
    for(int iext=0; iext<abs(nclk); iext++){hit_clock|=(hit_clock<<1);}
  }
  else{
    for(int iext=0; iext<abs(nclk); iext++){hit_clock&=(hit_clock>>1);}
  }

  return;
}

/////////////////////////////////////////////////////////////////////

void MutrgHit_v2::ShiftHitClock(int nclk){
  hit_clock=(nclk>0 ? hit_clock<<nclk : hit_clock>>abs(nclk));
  return;
}

/////////////////////////////////////////////////////////////////////

void MutrgHit_v2::print(ostream &os) const{
  int arm,st,oct,hoct,strip;
  MutrgKey::KeyToLoc(location,arm,st,oct,hoct,strip);

  os << "Location=" << setw(8) << setfill('0') << hex << location
     << dec << " (arm=" << arm << " st=" << st << " oct=" << oct
     << " hoct=" << hoct << " strip=" << strip << ")" << endl;

  os << "Hit Clock=" << setw(8) << setfill('0') << hex << hit_clock
     << " State=" << hex << state << endl;

  return;
}

/////////////////////////////////////////////////////////////////
