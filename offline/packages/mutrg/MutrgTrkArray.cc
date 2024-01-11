#include "MutrgTrkArray.hh"

#include <string>
#include <TClonesArray.h>
#include "MutrgTrk.hh"
#include "MutrgKey.hh"

using namespace std;

////////////////////////////////////////////////////////////

MutrgTrkArray::MutrgTrkArray(const char *mutrg_trk_class){
  if(string(mutrg_trk_class)==""){mutrg_trks=NULL;}
  else{
    mutrg_trks=new TClonesArray(mutrg_trk_class,100);
  }

  Reset();
}

////////////////////////////////////////////////////////////

MutrgTrkArray::~MutrgTrkArray(void){
  if(mutrg_trks){
    mutrg_trks->Delete();
    delete mutrg_trks;
    mutrg_trks=NULL;
  }
}

///////////////////////////////////////////////////////////

void MutrgTrkArray::Reset(void){
  if(mutrg_trks){mutrg_trks->Delete();}
  return;
}

///////////////////////////////////////////////////////////

MutrgTrk* MutrgTrkArray::Insert(void){
  if(!mutrg_trks){return NULL;}

  int idx_last=mutrg_trks->GetLast();
  MutrgTrk *trk=dynamic_cast<MutrgTrk*>(mutrg_trks->New(idx_last+1));

  return trk;
}

///////////////////////////////////////////////////////////

void MutrgTrkArray::Remove(MutrgTrk *trk){
  if(!mutrg_trks){
    printf("Error - %s::Remove : mutrg_trks=NULL\n",ClassName());
    return;
  }

  mutrg_trks->Remove(trk);
  mutrg_trks->Compress();
  return;
}

////////////////////////////////////////////////////////////

void MutrgTrkArray::Remove(int itrk){
  if(itrk<0 || itrk>=(int)GetSize()){
    printf("Error - %s::Remove : Out of range.\n",ClassName());
    return;
  }

  mutrg_trks->RemoveAt(itrk);
  mutrg_trks->Compress();
  return;
}

////////////////////////////////////////////////////////////

unsigned int MutrgTrkArray::GetSize(void){
  if(mutrg_trks){return mutrg_trks->GetLast()+1;}
  else          {return 0;}
}

////////////////////////////////////////////////////////////

MutrgTrk* MutrgTrkArray::Get(int itrk){
  if(mutrg_trks){return dynamic_cast<MutrgTrk*>(mutrg_trks->At(itrk));}
  else          {return NULL;}
}

////////////////////////////////////////////////////////////

void MutrgTrkArray::print(ostream &os) const{
  if(!mutrg_trks){
    printf("No mutrg_trks(TClonesArray)\n");
    return;
  }

  int ntrk=mutrg_trks->GetLast()+1;
  for(int itrk=0; itrk<ntrk; itrk++){
    MutrgTrk *trk=dynamic_cast<MutrgTrk*>(mutrg_trks->At(itrk));

    os << "MutrgTrk " << itrk
       << " -------------------------------" << endl;
    trk->print(os);
    os << "------------------------------------------" << endl;
  }

  return;
}

/////////////////////////////////////////////////////////////
