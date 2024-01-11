#include "PHMuoTrackAdc.h"

ClassImp(PHMuoTrackAdc);

///////////////////////////////////////////////////////////////////

bool PHMuoTrackAdc::CheckPlaneNum(int pl) const{
  if(pl<0 || pl>=MAX_CATHODE_PLANE){
    printf("Error - %s : Plane number (%d) is out of range.\n",ClassName(),pl);
    return false;
  }

  return true;
}

/////////////////////////////////////////////////////////////////////

bool PHMuoTrackAdc::CheckStripNum(int pl,int istrip) const{
  if(istrip<0 || istrip>=GetNStrip(pl)){
    printf("Error - %s : Strip number (%d) is out of range. (0 - %d)\n",
	   ClassName(),istrip,GetNStrip(pl));
    return false;
  }

  return true;
}

/////////////////////////////////////////////////////////////////////

bool PHMuoTrackAdc::CheckAdcNum(int iadc) const{
  if(iadc<0 || iadc>=N_ADC_SAMPLE){
    printf("Error - %s : ADC number (%d) is out of range. (0 - %d)\n",
	   ClassName(),iadc,N_ADC_SAMPLE);
    return false;
  }

  return true;
}

/////////////////////////////////////////////////////////////////////
