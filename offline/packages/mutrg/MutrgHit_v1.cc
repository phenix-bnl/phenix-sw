#include "MutrgHit_v1.hh"
#include "MutrgKey.hh"

#include <stdlib.h>
#include <sstream>
#include <iomanip>

using namespace std;

ClassImp(MutrgHit_v1);

//////////////////////////////////////////////////////////////

MutrgHit_v1::MutrgHit_v1(void){
  Reset();
}

//////////////////////////////////////////////////////////////

void MutrgHit_v1::Reset(void){
  location=0;
  hit_clock=0;
  state=0; // [1:mask_flag,0:dead_flag]
  for(int igap=0; igap<MutrgPar::MAX_NGAP; igap++){
    charge[igap]=-9999.0;
    charge_error[igap]=-9999.0;
    peak_time[igap]=-9999.0;
  }
  return;
}

///////////////////////////////////////////////////////////////

void MutrgHit_v1::Clear(Option_t*) {
  Reset();
}

///////////////////////////////////////////////////////////////

void MutrgHit_v1::SetLocation(int arm,int station,int octant,
			      int halfoctant,int gap,int cathode,
			      int strip){
  location=MutrgKey::LocToKey(arm,station,octant,halfoctant,gap,cathode,strip);
  return;
}

///////////////////////////////////////////////////////////////////

void MutrgHit_v1::SetLocation(int arm,int station,int octant,
			      int halfoctant,int strip){
  location=MutrgKey::LocToKey(arm,station,octant,halfoctant,strip);
  return;
}

///////////////////////////////////////////////////////////////////

void MutrgHit_v1::SetHitClock(unsigned short clk,bool hit){
  if(hit){hit_clock|= (0x1<<clk);}
  else   {hit_clock&=~(0x1<<clk);}
  return;
}

//////////////////////////////////////////////////////////////////

void MutrgHit_v1::SetDeadFlag(bool flag){
  state&=(((unsigned short)flag)<<0);
  return;
}

///////////////////////////////////////////////////////////////////

void MutrgHit_v1::SetMaskFlag(bool flag){
  state&=(((unsigned short)flag)<<1);
  return;
}

///////////////////////////////////////////////////////////////////

int MutrgHit_v1::SetMutHitValue(int gap,float q,float qerr,float ptime){
  if(gap<0 || gap>=MutrgPar::MAX_NGAP){
    stringstream ss;
    ss << "Error - " << ClassName() << " : gap=" << gap
       << " is out of range" << endl;
    Message(ss.str().c_str());
    return -1;
  }

  charge[gap]=q;
  charge_error[gap]=qerr;
  peak_time[gap]=ptime;
  return 0;
}

///////////////////////////////////////////////////////////////////

void MutrgHit_v1::SetMutMaxCharge(float q){
  // Nothing is done in MutrgHit_v1
  return;
}

////////////////////////////////////////////////////////////////////

void MutrgHit_v1::GetLocation(int &arm,int &station,int &octant,
			      int &halfoctant,int &gap,int &cathode,
			      int &strip) const{
  MutrgKey::KeyToLoc(location,arm,station,octant,halfoctant,gap,cathode,strip);
  return;
}

////////////////////////////////////////////////////////////////////

void MutrgHit_v1::GetLocation(int &arm,int &station,int &octant,
			      int &halfoctant,int &strip) const{
  int gap,cathode; // discard gap and cathode
  GetLocation(arm,station,octant,halfoctant,gap,cathode,strip);
  return;
}

////////////////////////////////////////////////////////////////////

bool MutrgHit_v1::GetHitClock(unsigned short clk) const{
  return (bool)(hit_clock&(0x1<<clk));
}

///////////////////////////////////////////////////////////////////

bool MutrgHit_v1::GetDeadFlag(void) const{
  return (bool)((state>>0)&0x1);
}

//////////////////////////////////////////////////////////////////

bool MutrgHit_v1::GetMaskFlag(void) const{
  return (bool)((state>>1)&0x1);
}

//////////////////////////////////////////////////////////////////

int MutrgHit_v1::GetMutHitValue(int gap,float &q,float &qerr,
				float &ptime) const{
  if(gap<0 || gap>=MutrgPar::MAX_NGAP){
    stringstream ss;
    ss << "Error - " << ClassName() << " : gap=" << gap
       << " is out of range" << endl;
    Message(ss.str().c_str());
    return -1;
  }

  q=charge[gap];
  qerr=charge_error[gap];
  ptime=peak_time[gap];
  return 0;
}

//////////////////////////////////////////////////////////////////

float MutrgHit_v1::GetMutMaxCharge(void) const{
  float charge_max=-9999.0;
  for(int igap=0; igap<MutrgPar::MAX_NGAP; igap++){
    if(charge_max<charge[igap]){charge_max=charge[igap];}
  }

  return charge_max;
}

//////////////////////////////////////////////////////////////////////

void MutrgHit_v1::ExtendHitClock(int nclk){
  if(nclk>0){
    for(int iext=0; iext<abs(nclk); iext++){hit_clock|=(hit_clock<<1);}
  }
  else{
    for(int iext=0; iext<abs(nclk); iext++){hit_clock&=(hit_clock>>1);}
  }

  return;
}

/////////////////////////////////////////////////////////////////////

void MutrgHit_v1::ShiftHitClock(int nclk){
  hit_clock=(nclk>0 ? hit_clock<<nclk : hit_clock>>abs(nclk));
  return;
}

/////////////////////////////////////////////////////////////////////

void MutrgHit_v1::print(ostream &os) const{
  int arm,st,oct,hoct,strip;
  MutrgKey::KeyToLoc(location,arm,st,oct,hoct,strip);

  os << "Location=" << setw(8) << setfill('0') << hex << location
     << dec << " (arm=" << arm << " st=" << st << " oct=" << oct
     << " hoct=" << hoct << " strip=" << strip << ")" << endl;

  os << "Hit Clock=" << setw(8) << setfill('0') << hex << hit_clock
     << " State=" << hex << state << endl;

  os << "MuTr Hit : ";
  for(int igap=0; igap<MutrgPar::MAX_NGAP; igap++){
    if(igap!=0){os << "           ";}
    os << "Charge " << igap << " ="
       << setprecision(4) << charge[igap] << " +- "
       << setprecision(4) << charge_error[igap] << "  Peak Time="
       << setprecision(4) << peak_time[igap] << endl;
  }

  return;
}

/////////////////////////////////////////////////////////////////
