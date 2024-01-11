#include "PHMuoTracksAdcv1.h"

#include <string>

using namespace std;

ClassImp(PHMuoTracksAdcv1);

////////////////////////////////////////////////////////////

PHMuoTracksAdcv1::PHMuoTracksAdcv1(const char *content_class) :
  PHMuoTracksAdc(content_class){
}

/////////////////////////////////////////////////////////////

PHMuoTracksAdcv1::~PHMuoTracksAdcv1(void){;}

/////////////////////////////////////////////////////////////

PHMuoTracksAdc* PHMuoTracksAdcv1::Create(const char *content_class){
  const char *cname=GetContentName();
  if(string(content_class)=="" && cname){content_class=cname;}
  return new PHMuoTracksAdcv1(content_class);
}

//////////////////////////////////////////////////////////////
