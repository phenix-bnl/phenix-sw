#include "MutrgTrk_v1.hh"

#include <iomanip>
#include "MutrgKey.hh"

using namespace std;

ClassImp(MutrgTrk_v1);

//////////////////////////////////////////////////////////

MutrgTrk_v1::MutrgTrk_v1(void){
  MAX_INDEX_MUTR=0;
  index_mutr=NULL;
  Reset();
}

/////////////////////////////////////////////////////////

MutrgTrk_v1::~MutrgTrk_v1(void){
  if(index_mutr){delete [] index_mutr;}
  index_mutr=NULL;
}

//////////////////////////////////////////////////////////

void MutrgTrk_v1::Reset(void){
  nindex_mutr=0;
  for(int ist=0; ist<MutrgPar::NSTATION; ist++){hit_location[ist]=~0;}
}

//////////////////////////////////////////////////////////

void MutrgTrk_v1::ClearMuTrIndex(void){
  nindex_mutr=0;
  return;
}

/////////////////////////////////////////////////////////

void MutrgTrk_v1::ClearMuTrUid(void){
  // This function is unavailable in MutrgTrk_v1
  return;
}

/////////////////////////////////////////////////////////

void MutrgTrk_v1::AddMuTrIndex(unsigned short value){
  if(MAX_INDEX_MUTR<=nindex_mutr){
    MAX_INDEX_MUTR+=10;

    if(index_mutr){
      UShort_t *index_temp=index_mutr;
      index_mutr=new UShort_t[MAX_INDEX_MUTR];
      for(int i=0; i<nindex_mutr; i++){index_mutr[i]=index_temp[i];}
      delete [] index_temp;
    }
    else{
      index_mutr=new UShort_t[MAX_INDEX_MUTR];
    }
  }

  index_mutr[nindex_mutr]=value;
  nindex_mutr++;

  return;
}

////////////////////////////////////////////////////////

void MutrgTrk_v1::AddMuTrUid(unsigned int value){
  // This function is unavailable in MutrgTrk_v1
  return;
}

////////////////////////////////////////////////////////

unsigned short MutrgTrk_v1::GetMuTrIndex(int num) const {
  if(num<0 || num>=nindex_mutr){
    printf("Error - %s::GetMuTrIndex : Out of range\n",ClassName());
    return ~0;
  }

  return index_mutr[num];
}

/////////////////////////////////////////////////////////

int MutrgTrk_v1::GetMuTrNUid(void) const {
  // This function is unavailable in MutrgTrk_v1
  return -1;
}

/////////////////////////////////////////////////////////

unsigned int MutrgTrk_v1::GetMuTrUid(int num) const {
  // This function is unavailable in MutrgTrk_v1
  return 0;
}

/////////////////////////////////////////////////////////

int MutrgTrk_v1::SetHit(int station,unsigned int hit_loc){
  if(!CheckStation(station)){return -1;}
  hit_location[station]=hit_loc;
  return 0;
}

//////////////////////////////////////////////////////////

void MutrgTrk_v1::SetHitClock(unsigned short hit){
  // This function is unavailable in MutrgTrk_v1
  return;
}

//////////////////////////////////////////////////////////

void MutrgTrk_v1::SetHitClock(unsigned short clk,bool hit){
  // This function is unavailable in MutrgTrk_v1
  return;
}

//////////////////////////////////////////////////////////

unsigned int MutrgTrk_v1::GetHit(int station) const {
  if(!CheckStation(station)){return ~0;}
  return hit_location[station];
}

///////////////////////////////////////////////////////////

unsigned short MutrgTrk_v1::GetHitClock(void) const {
  // This function is unavailable in MutrgTrk_v1
  return 0;
}

///////////////////////////////////////////////////////////

bool MutrgTrk_v1::GetHitClock(unsigned short clk) const {
  // This function is unavailable in MutrgTrk_v1
  return false;
}

///////////////////////////////////////////////////////////

void MutrgTrk_v1::print(std::ostream &os) const{
  for(int idx=0; idx<nindex_mutr; idx++){
    os << "Index MuTr : " << hex << setw(4)
       << setfill('0') << index_mutr[idx] << endl;
  }

  if(nindex_mutr==0){
    os << "Index MuTr : No association" << endl;
  }

  for(int ist=0; ist<MutrgPar::NSTATION; ist++){
    int arm,st,oct,hoct,strip;
    MutrgKey::KeyToLoc(hit_location[ist],arm,st,oct,hoct,strip);

    os << "St " << dec << ist << " : " << setfill('0')
       << hex << setw(8) << hit_location[ist] << dec
       << " (arm=" << arm << " st=" << st << " oct=" << oct
       << " hoct=" << hoct << " strip=" << strip << ")" << endl;
  }

  return;
}

///////////////////////////////////////////////////////////
