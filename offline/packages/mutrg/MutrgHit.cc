#include "MutrgHit.hh"

#include <string>

#include "MutrgPar.hh"

using namespace std;

ClassImp(MutrgHit);

//////////////////////////////////////////////////////////////

void MutrgHit::Set(const MutrgHit *hit_org){
  SetKey(hit_org->GetKey());
  SetHitClock(hit_org->GetHitClock());
  SetState(hit_org->GetState());
  for(int igap=0; igap<MutrgPar::MAX_NGAP; igap++){
    float q,qerr,ptime;
    hit_org->GetMutHitValue(igap,q,qerr,ptime);
    SetMutHitValue(igap,q,qerr,ptime);
  }
  SetMutMaxCharge(hit_org->GetMutMaxCharge());

  return;
}

///////////////////////////////////////////////////////////////

void MutrgHit::Message(const char *msg,std::ostream &os){
  os << msg;
  return;
}

///////////////////////////////////////////////////////////////////

void MutrgHit::print(std::ostream &os) const{
  os << "Warning - " << ClassName()
     << "::print : Defalut function called\n" << endl;
  return;
}

////////////////////////////////////////////////////////////////////
