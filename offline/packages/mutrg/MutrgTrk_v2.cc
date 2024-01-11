#include "MutrgTrk_v2.hh"

#include <iomanip>
#include "MutrgKey.hh"

using namespace std;

ClassImp(MutrgTrk_v2);

//////////////////////////////////////////////////////////

MutrgTrk_v2::MutrgTrk_v2(void){
  MAX_INDEX_MUTR=0;
  index_mutr=NULL;
  Reset();
}

/////////////////////////////////////////////////////////

MutrgTrk_v2::~MutrgTrk_v2(void){
  if(index_mutr){delete [] index_mutr;}
  index_mutr=NULL;
}

//////////////////////////////////////////////////////////

void MutrgTrk_v2::Reset(void){
  nindex_mutr=0;
  for(int ist=0; ist<MutrgPar::NSTATION; ist++){hit_location[ist]=~0;}
}

//////////////////////////////////////////////////////////

void MutrgTrk_v2::ClearMuTrIndex(void){
  nindex_mutr=0;
  return;
}

/////////////////////////////////////////////////////////

void MutrgTrk_v2::ClearMuTrUid(void){
  // This function is unavailable in MutrgTrk_v2
  return;
}

/////////////////////////////////////////////////////////

void MutrgTrk_v2::AddMuTrIndex(unsigned short value){
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

void MutrgTrk_v2::AddMuTrUid(unsigned int value){
  // This function is unavailable in MutrgTrk_v2
  return;
}

////////////////////////////////////////////////////////

int MutrgTrk_v2::SetHit(int station,unsigned int hit_loc){
  if(!CheckStation(station)){return -1;}
  hit_location[station]=hit_loc;
  return 0;
}

//////////////////////////////////////////////////////////

void MutrgTrk_v2::SetHitClock(unsigned short clk,bool hit){
  if(hit){hit_clock|= (0x1<<clk);}
  else   {hit_clock&=~(0x1<<clk);}
  return;
}

//////////////////////////////////////////////////////////////////

unsigned short MutrgTrk_v2::GetMuTrIndex(int num) const {
  if(num<0 || num>=nindex_mutr){
    printf("Error - %s::GetMuTrIndex : Out of range\n",ClassName());
    return ~0;
  }

  return index_mutr[num];
}

/////////////////////////////////////////////////////////

int MutrgTrk_v2::GetMuTrNUid(void) const {
  // This function is unavailable in MutrgTrk_v2
  return -1;
}

/////////////////////////////////////////////////////////

unsigned int MutrgTrk_v2::GetMuTrUid(int num) const {
  // This function is unavailable in MutrgTrk_v2
  return 0;
}

/////////////////////////////////////////////////////////

unsigned int MutrgTrk_v2::GetHit(int station) const {
  if(!CheckStation(station)){return ~0;}
  return hit_location[station];
}

///////////////////////////////////////////////////////////

bool MutrgTrk_v2::GetHitClock(unsigned short clk) const {
  return (bool)(hit_clock&(0x1<<clk));
}

///////////////////////////////////////////////////////////////////

void MutrgTrk_v2::print(std::ostream &os) const {
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

  os << "Hit Clock : ";
  for(int iclk=MutrgPar::MAX_NHITCLOCK-1; iclk>-1; iclk--){
    os << (int)((hit_clock&(0x1<<iclk))>0);
  }
  os << " (0x" << setfill('0') << hex << setw(2) << hit_clock << ")" << endl;

  return;
}

///////////////////////////////////////////////////////////
