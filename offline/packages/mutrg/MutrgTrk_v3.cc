#include "MutrgTrk_v3.hh"

#include <iomanip>
#include "MutrgKey.hh"

using namespace std;

ClassImp(MutrgTrk_v3);

//////////////////////////////////////////////////////////

MutrgTrk_v3::MutrgTrk_v3(void){
  MAX_UID_MUTR=0;
  uid_mutr=NULL;
  Reset();
}

/////////////////////////////////////////////////////////

MutrgTrk_v3::~MutrgTrk_v3(void){
  if(uid_mutr){delete [] uid_mutr;}
  uid_mutr=NULL;
}

//////////////////////////////////////////////////////////

void MutrgTrk_v3::Reset(void){
  nuid_mutr=0;
  for(int ist=0; ist<MutrgPar::NSTATION; ist++){hit_location[ist]=~0;}
}

//////////////////////////////////////////////////////////

void MutrgTrk_v3::ClearMuTrIndex(void){
  // This function is unavailable in MutrgTrk_v3
  return;
}

/////////////////////////////////////////////////////////////

void MutrgTrk_v3::ClearMuTrUid(void){
  nuid_mutr=0;
  return;
}

/////////////////////////////////////////////////////////

void MutrgTrk_v3::AddMuTrIndex(unsigned short value){
  // This function is unavailable in MutrgTrk_v3
  return;
}

////////////////////////////////////////////////////////////

void MutrgTrk_v3::AddMuTrUid(unsigned int value){
  if(MAX_UID_MUTR<=nuid_mutr){
    MAX_UID_MUTR+=10;

    if(uid_mutr){
      unsigned int *uid_temp=uid_mutr;
      uid_mutr=new unsigned int[MAX_UID_MUTR];
      for(int i=0; i<nuid_mutr; i++){uid_mutr[i]=uid_temp[i];}
      delete [] uid_temp;
    }
    else{
      uid_mutr=new unsigned int[MAX_UID_MUTR];
    }
  }

  uid_mutr[nuid_mutr]=value;
  nuid_mutr++;

  return;
}

////////////////////////////////////////////////////////

int MutrgTrk_v3::GetMuTrNIndex(void) const{
  // This function is unavailable in MutrgTrk_v3
  return -1;
}

//////////////////////////////////////////////////////////

unsigned short MutrgTrk_v3::GetMuTrIndex(int num) const{
  // This function is unavailable in MutrgTrk_v3
  return 0;
}

///////////////////////////////////////////////////////////

unsigned int MutrgTrk_v3::GetMuTrUid(int num) const{
  if(num<0 || num>=nuid_mutr){
    printf("Error - %s::GetMuTrUid : Out of range\n",ClassName());
    return ~0;
  }

  return uid_mutr[num];

  return 0;
}

/////////////////////////////////////////////////////////

int MutrgTrk_v3::SetHit(int station,unsigned int hit_loc){
  if(!CheckStation(station)){return -1;}
  hit_location[station]=hit_loc;
  return 0;
}

//////////////////////////////////////////////////////////

void MutrgTrk_v3::SetHitClock(unsigned short clk,bool hit){
  if(hit){hit_clock|= (0x1<<clk);}
  else   {hit_clock&=~(0x1<<clk);}
  return;
}

//////////////////////////////////////////////////////////////////

unsigned int MutrgTrk_v3::GetHit(int station) const {
  if(!CheckStation(station)){return ~0;}
  return hit_location[station];
}

///////////////////////////////////////////////////////////

bool MutrgTrk_v3::GetHitClock(unsigned short clk) const {
  return (bool)(hit_clock&(0x1<<clk));
}

///////////////////////////////////////////////////////////////////

void MutrgTrk_v3::print(std::ostream &os) const {
  for(int idx=0; idx<nuid_mutr; idx++){
    os << "UID MuTr : " << hex << setw(4)
       << setfill('0') << uid_mutr[idx] << endl;
  }

  if(nuid_mutr==0){
    os << "UID MuTr : No association" << endl;
  }

  for(int ist=0; ist<MutrgPar::NSTATION; ist++){
    int arm,st,oct,hoct,strip;
    MutrgKey::KeyToLoc(hit_location[ist],arm,st,oct,hoct,strip);

    os << "St " << dec << ist << " : " << setfill('0')
       << hex << setw(8) << hit_location[ist] << dec
       << " (arm=" << arm << " st=" << st << " oct=" << oct
       << " hoct=" << hoct << " strip=" << strip << ")" << endl;
  }

  os << "Hit Clock : ";
  for(int iclk=MutrgPar::MAX_NHITCLOCK-1; iclk>-1; iclk--){
    os << (int)((hit_clock&(0x1<<iclk))>0);
  }
  os << " (0x" << setfill('0') << hex << setw(2) << hit_clock << ")" << endl;

  return;
}

///////////////////////////////////////////////////////////
