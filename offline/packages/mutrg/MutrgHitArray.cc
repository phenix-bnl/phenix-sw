#include "MutrgHitArray.hh"

#include <TClass.h>
#include <TClonesArray.h>

#include "MutrgHit.hh"
#include "MutrgKey.hh"

using namespace std;

ClassImp(MutrgHitArray);

/////////////////////////////////////////////////////////////////

MutrgHitArray::MutrgHitArray(const char *mutrg_hit_class){
  if(string(mutrg_hit_class)==""){mutrg_hits=NULL;}
  else{mutrg_hits=new TClonesArray(mutrg_hit_class,100);}
  mutrg_hitmap.clear();
}

/////////////////////////////////////////////////////////////////

MutrgHitArray::~MutrgHitArray(void){
  if(mutrg_hits){
    mutrg_hits->Delete();
    delete mutrg_hits;
    mutrg_hits=NULL;
  }

  mutrg_hitmap.clear();
}

/////////////////////////////////////////////////////////////////

MutrgHitArray* MutrgHitArray::Create(const char *mutrg_hit_class){
  if(!mutrg_hit_class){
    printf("Error %s::Create : mutrg_hit_clss=NULL\n",ClassName());
    return NULL;
  }

  const char *cname=GetMutrgHitName();
  if(string(mutrg_hit_class)=="" && cname){mutrg_hit_class=cname;}
  return new MutrgHitArray(mutrg_hit_class);
}

/////////////////////////////////////////////////////////////////

void MutrgHitArray::Reset(void){
  if(mutrg_hits){mutrg_hits->Delete();}
  mutrg_hitmap.clear();
  return;
}

///////////////////////////////////////////////////////////////

int MutrgHitArray::Set(const MutrgHitArray *mutrg_hits_org){
  Clear();

  MutrgHitArray::const_private_itr_pair itr_pair=mutrg_hits_org->Range();
  MutrgHitArray::const_private_itr itr_beg=itr_pair.first;
  MutrgHitArray::const_private_itr itr_end=itr_pair.second;
  for(MutrgHitArray::const_private_itr itr=itr_beg; itr!=itr_end; itr++){
    unsigned int key=itr->first;
    const MutrgHit *hit_org=itr->second;
    MutrgHit *hit_copy=Insert(key);
    hit_copy->Set(hit_org);
  }

  return 0;
}

///////////////////////////////////////////////////////////////

int MutrgHitArray::Readback(void){
  if(!mutrg_hits){
    printf("Error %s::Readback : mutrg_hits(TClonesArray)=NULL\n",ClassName());
    return -1;
  }

  if(mutrg_hitmap.size()!=0 &&
     mutrg_hits->GetEntries()!=(int)mutrg_hitmap.size()){
    printf("Error %s::Readback : Inconsistent size\n",ClassName());
    return -1;
  }

  if(mutrg_hits->GetEntries()!=mutrg_hits->GetLast()+1){
    mutrg_hits->Compress();
  }

  if(mutrg_hitmap.size()==0){
    for(int ihit=0; ihit<mutrg_hits->GetEntries(); ihit++){
      MutrgHit *mhit=dynamic_cast<MutrgHit*>(mutrg_hits->At(ihit));
      mutrg_hitmap.insert(pair<unsigned int,MutrgHit*>(mhit->GetKey(),mhit));
    }
  }

  return 0;
}

///////////////////////////////////////////////////////////////

MutrgHit* MutrgHitArray::Insert(unsigned int key){
  private_itr itr=mutrg_hitmap.find(key);
  if(itr!=mutrg_hitmap.end()){return itr->second;}

  int idx_last=mutrg_hits->GetLast();
  MutrgHit *hit=dynamic_cast<MutrgHit*>(mutrg_hits->New(idx_last+1));
  hit->SetKey(key);

  mutrg_hitmap.insert(pair<unsigned int,MutrgHit*>(key,hit));

  return hit;
}

////////////////////////////////////////////////////////////////

MutrgHit* MutrgHitArray::Insert(int arm,int station,int octant,
				int halfoctant,int strip){
  unsigned int key=MutrgKey::LocToKey(arm,station,octant,halfoctant,strip);
  return Insert(key);
}

///////////////////////////////////////////////////////////////

MutrgHit* MutrgHitArray::Insert(int arm,int station,int octant,
				int halfoctant,int gap,int cathode,int strip){
  unsigned int key=MutrgKey::LocToKey(arm,station,octant,
				      halfoctant,gap,cathode,strip);
  return Insert(key);
}

//////////////////////////////////////////////////////////////

MutrgHit* MutrgHitArray::InsertCheck(unsigned int key){
  if(Readback()){return NULL;}
  return Insert(key);
}

//////////////////////////////////////////////////////////////

MutrgHit* MutrgHitArray::InsertCheck(int arm,int station,int octant,
				     int halfoctant,int strip){
  if(Readback()){return NULL;}
  return Insert(arm,station,octant,halfoctant,strip);
}

///////////////////////////////////////////////////////////////

MutrgHit* MutrgHitArray::InsertCheck(int arm,int station,int octant,
				     int halfoctant,int gap,int cathode,
				     int strip){
  if(Readback()){return NULL;}
  return Insert(arm,station,octant,halfoctant,gap,cathode,strip);
}

////////////////////////////////////////////////////////////////

MutrgHit *MutrgHitArray::Find(unsigned int key) const{
  const_private_itr itr=mutrg_hitmap.find(key);
  if(itr==mutrg_hitmap.end()){return NULL;}
  return itr->second;
}

/////////////////////////////////////////////////////////////////

MutrgHitArray::const_private_itr MutrgHitArray::Begin(void) const{
  return mutrg_hitmap.begin();
}

//////////////////////////////////////////////////////////////////

MutrgHitArray::const_private_itr MutrgHitArray::End(void) const{
  return mutrg_hitmap.end();
}

//////////////////////////////////////////////////////////////////

MutrgHitArray::const_private_itr_pair MutrgHitArray::Range(void) const{
  return const_private_itr_pair(mutrg_hitmap.begin(),mutrg_hitmap.end());
}

////////////////////////////////////////////////////////////////

MutrgHitArray::const_private_itr_pair MutrgHitArray::Range(int arm) const{
  unsigned int key_beg=MutrgKey::LocToKey(arm,0,0,0,0,0,0);
  unsigned int key_end=MutrgKey::LocToKey(arm+1,0,0,0,0,0,0)-1;
  const_private_itr itr_beg=mutrg_hitmap.lower_bound(key_beg);
  const_private_itr itr_end=mutrg_hitmap.upper_bound(key_end);
  return const_private_itr_pair(itr_beg,itr_end);
}

///////////////////////////////////////////////////////////////

MutrgHitArray::const_private_itr_pair
MutrgHitArray::Range(int arm,int station) const{
  unsigned int key_beg=MutrgKey::LocToKey(arm,station,0,0,0,0,0);
  unsigned int key_end=MutrgKey::LocToKey(arm,station+1,0,0,0,0,0)-1;
  const_private_itr itr_beg=mutrg_hitmap.lower_bound(key_beg);
  const_private_itr itr_end=mutrg_hitmap.upper_bound(key_end);
  return const_private_itr_pair(itr_beg,itr_end);
}

///////////////////////////////////////////////////////////////

MutrgHitArray::const_private_itr_pair
MutrgHitArray::Range(int arm,int station,int octant,int halfoctant) const{
  unsigned int key_beg=MutrgKey::LocToKey(arm,station,octant,halfoctant,0,0,0);
  unsigned int key_end=MutrgKey::LocToKey(arm,station,octant,
					  halfoctant+1,0,0,0)-1;
  const_private_itr itr_beg=mutrg_hitmap.lower_bound(key_beg);
  const_private_itr itr_end=mutrg_hitmap.upper_bound(key_end);
  return const_private_itr_pair(itr_beg,itr_end);
}

////////////////////////////////////////////////////////////////

void MutrgHitArray::Remove(unsigned int key){
  map<unsigned int,MutrgHit*>::iterator itr=mutrg_hitmap.find(key);
  if(itr==mutrg_hitmap.end()){return;}

  MutrgHit *hit=itr->second;

  mutrg_hits->Remove(hit);
  mutrg_hits->Compress();

  mutrg_hitmap.erase(itr);
  return;
}

/////////////////////////////////////////////////////////////////

void MutrgHitArray::Remove(const MutrgHit *hit){
  Remove(hit->GetKey());
  return;
}

/////////////////////////////////////////////////////////////////

void MutrgHitArray::ExtendHitClock(int nclk){
  const_private_itr_pair itr_pair=Range();
  const_private_itr itr_beg=itr_pair.first;
  const_private_itr itr_end=itr_pair.second;
  for(const_private_itr itr=itr_beg; itr!=itr_end; itr++){
    MutrgHit *hit=itr->second;
    hit->ExtendHitClock(nclk);
  }

  return;
}

/////////////////////////////////////////////////////////////////

void MutrgHitArray::ShiftHitClock(int nclk){
  const_private_itr_pair itr_pair=Range();
  const_private_itr itr_beg=itr_pair.first;
  const_private_itr itr_end=itr_pair.second;
  for(const_private_itr itr=itr_beg; itr!=itr_end; itr++){
    MutrgHit *hit=itr->second;
    hit->ShiftHitClock(nclk);
  }

  return;
}

/////////////////////////////////////////////////////////////////

void MutrgHitArray::print(ostream &os) const{
  int nhit=mutrg_hits->GetLast()+1;
  for(int ihit=0; ihit<nhit; ihit++){
    MutrgHit *hit=dynamic_cast<MutrgHit*>(mutrg_hits->At(ihit));

    os << "MutrgHit " << nhit
       << " -------------------------------" << endl;
    hit->print(os);
    os << "------------------------------------------" << endl;
  }

  return;
}

/////////////////////////////////////////////////////////////////

const char* MutrgHitArray::GetMutrgHitName(void){
  if(mutrg_hits){return mutrg_hits->GetClass()->GetName();}
  return NULL;
}

///////////////////////////////////////////////////////////////////
