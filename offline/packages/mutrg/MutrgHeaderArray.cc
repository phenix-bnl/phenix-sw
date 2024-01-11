#include "MutrgHeaderArray.hh"

#include <string>
#include <TClass.h>
#include <TClonesArray.h>

#include "MutrgHeader.hh"

ClassImp(MutrgHeaderArray);

using namespace std;

////////////////////////////////////////////////////////////

MutrgHeaderArray::MutrgHeaderArray(const char *mutrg_header_class){
  if(string(mutrg_header_class)==""){mutrg_headers=NULL;}
  else{mutrg_headers=new TClonesArray(mutrg_header_class,10);}
}

///////////////////////////////////////////////////////////

MutrgHeaderArray::~MutrgHeaderArray(void){
  if(mutrg_headers){
    mutrg_headers->Delete();
    delete mutrg_headers;
    mutrg_headers=NULL;
  }
}

///////////////////////////////////////////////////////////

MutrgHeaderArray* MutrgHeaderArray::Create(const char *mutrg_header_class){
  if(!mutrg_header_class){
    printf("Error %s::Create : mutrg_header_clss=NULL\n",ClassName());
    return NULL;
  }

  const char *cname=GetMutrgHeaderName();
  if(string(mutrg_header_class)=="" && cname){mutrg_header_class=cname;}
  return new MutrgHeaderArray(mutrg_header_class);
}

//////////////////////////////////////////////////////////

void MutrgHeaderArray::Reset(void){
  if(mutrg_headers){mutrg_headers->Delete();}
  return;
}

///////////////////////////////////////////////////////////

MutrgHeader* MutrgHeaderArray::Insert(void){
  if(!mutrg_headers){
    printf("Error - %s::Insert : mutrg_headers=NULL\n",ClassName());
    return NULL;
  }

  int idx_last=mutrg_headers->GetLast();
  MutrgHeader *mh=dynamic_cast<MutrgHeader*>(mutrg_headers->New(idx_last+1));

  return mh;
}

//////////////////////////////////////////////////////////

void MutrgHeaderArray::Remove(MutrgHeader *header){
  if(!mutrg_headers){
    printf("Error - %s::Remove : mutrg_headers=NULL\n",ClassName());
    return;
  }

  mutrg_headers->Remove(header);
  mutrg_headers->Compress();
  return;
}

/////////////////////////////////////////////////////////

void MutrgHeaderArray::Remove(int iheader){
  if(iheader<0 || iheader>=(int)GetSize()){
    printf("Error - %s::Remove : Out of range.\n",ClassName());
    return;
  }

  mutrg_headers->RemoveAt(iheader);
  mutrg_headers->Compress();
  return;
}

////////////////////////////////////////////////////////////

unsigned int MutrgHeaderArray::GetSize(void){
  if(mutrg_headers){return mutrg_headers->GetLast()+1;}
  else             {return 0;}
}

////////////////////////////////////////////////////////////

MutrgHeader* MutrgHeaderArray::Get(int iheader){
  if(mutrg_headers){
    return dynamic_cast<MutrgHeader*>(mutrg_headers->At(iheader));}
  else{return NULL;}
}

////////////////////////////////////////////////////////////

const char* MutrgHeaderArray::GetMutrgHeaderName(void){
  if(mutrg_headers){return mutrg_headers->GetClass()->GetName();}
  return NULL;
}

/////////////////////////////////////////////////////////////

