#include "PHMuoTracksAdc.h"

#include <string>
#include <TClass.h>
#include <TClonesArray.h>

#include "PHMuoTrackAdc.h"
#include "PHMuoTracksAdcv1.h"

using namespace std;

ClassImp(PHMuoTracksAdc);

/////////////////////////////////////////////////////////////////

PHMuoTracksAdc::PHMuoTracksAdc(const char *content_class){
  if(string(content_class)==""){muotrks_adc=NULL;}
  else{muotrks_adc=new TClonesArray(content_class,10);}
}

/////////////////////////////////////////////////////////////////

PHMuoTracksAdc::~PHMuoTracksAdc(void){
  if(muotrks_adc){
    muotrks_adc->Delete();
    delete muotrks_adc;
    muotrks_adc=NULL;
  }
}

/////////////////////////////////////////////////////////////////

PHMuoTracksAdc* PHMuoTracksAdc::Create(const char *content_class){
  const char *cname=GetContentName();
  if(string(content_class)=="" && cname){content_class=cname;}
  return new PHMuoTracksAdc(content_class);
}

/////////////////////////////////////////////////////////////////

void PHMuoTracksAdc::Reset(void){
  if(muotrks_adc){muotrks_adc->Delete();}
  return;
}

/////////////////////////////////////////////////////////////////

PHMuoTrackAdc* PHMuoTracksAdc::Insert(void){
  if(!muotrks_adc){
    printf("Error - %s::Insert : muotrks_adc=NULL\n",ClassName());
    return NULL;
  }

  int idx_last=muotrks_adc->GetLast();
  return dynamic_cast<PHMuoTrackAdc*>(muotrks_adc->New(idx_last+1));
}

/////////////////////////////////////////////////////////////////

void PHMuoTracksAdc::Remove(PHMuoTrackAdc *trk_adc){
  if(!muotrks_adc){
    printf("Error - %s::Remove : muotrks_adc=NULL\n",ClassName());
    return;
  }

  muotrks_adc->Remove(trk_adc);
  muotrks_adc->Compress();
  return;
}

/////////////////////////////////////////////////////////////////

void PHMuoTracksAdc::Remove(int itrk){
  if(itrk<0 || itrk>=(int)GetSize()){
    printf("Error - %s::Remove : Out of range.\n",ClassName());
    return;
  }

  muotrks_adc->RemoveAt(itrk);
  muotrks_adc->Compress();
  return;
}

/////////////////////////////////////////////////////////////////

unsigned int PHMuoTracksAdc::GetSize(void){
  if(muotrks_adc){return muotrks_adc->GetLast()+1;}
  else           {return 0;}
}

/////////////////////////////////////////////////////////////////

PHMuoTrackAdc* PHMuoTracksAdc::Get(int itrk){
  if(muotrks_adc){
    return dynamic_cast<PHMuoTrackAdc*>(muotrks_adc->At(itrk));
  }
  else{return NULL;}
}

/////////////////////////////////////////////////////////////////

const char* PHMuoTracksAdc::GetContentName(void){
  if(muotrks_adc){return muotrks_adc->GetClass()->GetName();}
  return NULL;
}

/////////////////////////////////////////////////////////////////

PHMuoTracksAdc* PHMuoTracksAdc::NewPHMuoTracksAdc(void){
  return new PHMuoTracksAdcv1("PHMuoTrackAdcv1");
}

////////////////////////////////////////////////////////////////
