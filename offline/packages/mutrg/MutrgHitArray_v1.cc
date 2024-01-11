#include "MutrgHitArray_v1.hh"

#include <string>

using namespace std;

ClassImp(MutrgHitArray_v1);

/////////////////////////////////////////////////////////////////

MutrgHitArray_v1::MutrgHitArray_v1(const char *mutrg_hit_class) :
  MutrgHitArray(mutrg_hit_class){
}

//////////////////////////////////////////////////////////////////

MutrgHitArray_v1::~MutrgHitArray_v1(void){;}

/////////////////////////////////////////////////////////////////

MutrgHitArray* MutrgHitArray_v1::Create(const char *mutrg_hit_class){
  if(!mutrg_hit_class){
    printf("Error %s::Create : mutrg_hit_clss=NULL\n",ClassName());
    return NULL;
  }

  const char *cname=GetMutrgHitName();
  if(string(mutrg_hit_class)=="" && cname){mutrg_hit_class=cname;}
  return new MutrgHitArray_v1(mutrg_hit_class);
}

//////////////////////////////////////////////////////////////////
