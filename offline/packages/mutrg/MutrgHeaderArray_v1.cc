#include "MutrgHeaderArray_v1.hh"

#include <string>

using namespace std;

ClassImp(MutrgHeaderArray_v1);

/////////////////////////////////////////////////////////

MutrgHeaderArray_v1::MutrgHeaderArray_v1(const char *mutrg_header_class) :
  MutrgHeaderArray(mutrg_header_class){
}

/////////////////////////////////////////////////////////

MutrgHeaderArray* MutrgHeaderArray_v1::Create(const char *mutrg_header_class){
  if(!mutrg_header_class){
    printf("Error %s::Create : mutrg_header_clss=NULL\n",ClassName());
    return NULL;
  }

  const char *cname=GetMutrgHeaderName();
  if(string(mutrg_header_class)=="" && cname){mutrg_header_class=cname;}
  return new MutrgHeaderArray_v1(mutrg_header_class);
}

//////////////////////////////////////////////////////////
