#include "PHMuoTrackAdcv1.h"

using namespace std;

ClassImp(PHMuoTrackAdcv1);

////////////////////////////////////////////////////////////

PHMuoTrackAdcv1::PHMuoTrackAdcv1(void){
  raw_adc=new unsigned short[MAX_ADC_HIT];
  Reset();
}

//////////////////////////////////////////////////////////////

PHMuoTrackAdcv1::~PHMuoTrackAdcv1(void){
  if(raw_adc){delete [] raw_adc;}
  raw_adc=NULL;
}

//////////////////////////////////////////////////////////////

void PHMuoTrackAdcv1::Reset(void){
  track_uid=0;
  n_adc_hit_total=0;
  for(int ipl=0; ipl<MAX_CATHODE_PLANE; ipl++){
    n_adc_hit[ipl]=0;
    hit_strip[ipl]=0;
  }

  for(int ihit=0; ihit<MAX_ADC_HIT; ihit++){
    raw_adc[ihit]=0;
  }

  return;
}

////////////////////////////////////////////////////////////

int PHMuoTrackAdcv1::SetAdc(vector<unsigned char> strip_array,
			     vector<vector<unsigned short> > adc_array){
  if(strip_array.size()!=MAX_CATHODE_PLANE ||
     adc_array.size()!=MAX_CATHODE_PLANE){
    printf("Error - %s : Array size (%d,%d) is not MAX_CATHODE_PLANE (%d)",
	   ClassName(),(int)strip_array.size(),(int)adc_array.size(),
	   MAX_CATHODE_PLANE);
    return -1;
  }

  for(unsigned int ipl=0; ipl<strip_array.size(); ipl++){
    hit_strip[ipl]=strip_array[ipl];
  }

  n_adc_hit_total=0;
  for(unsigned int ipl=0; ipl<adc_array.size(); ipl++){
    n_adc_hit[ipl]=(unsigned char)adc_array[ipl].size();
    for(unsigned int ihit=0; ihit<adc_array[ipl].size(); ihit++){
      raw_adc[n_adc_hit_total]=adc_array[ipl][ihit];
      n_adc_hit_total++;

      if(n_adc_hit_total>=MAX_ADC_HIT){
	printf("Error - %s : # of hits exceeds MAX_ADC_HIT\n",ClassName());
	return -1;
      }
    }
  }

  return 0;
}

///////////////////////////////////////////////////////////

void PHMuoTrackAdcv1::GetAdc(vector<unsigned char> &strip_array,
			      vector<vector<unsigned short> > &adc_array) const{
  strip_array.clear();
  adc_array.clear();

  for(int ipl=0; ipl<MAX_CATHODE_PLANE; ipl++){
    strip_array.push_back(hit_strip[ipl]);
  }

  int ihit_tot=0;
  for(int ipl=0; ipl<MAX_CATHODE_PLANE; ipl++){
    vector<unsigned short> adc;
    for(int ihit=0; ihit<n_adc_hit[ipl]; ihit++){
      adc.push_back(raw_adc[ihit_tot]);
      ihit_tot++;
    }

    adc_array.push_back(adc);
  }

  return;
}

///////////////////////////////////////////////////////////

unsigned char PHMuoTrackAdcv1::GetNStrip(int pl) const{
  if(!CheckPlaneNum(pl)){return 0;}
  return n_adc_hit[pl]/N_ADC_SAMPLE;
}

////////////////////////////////////////////////////////////////

unsigned char PHMuoTrackAdcv1::GetNStrip(int st,int gap,int cath) const{
  int pl=cath+gap*2+st*6;
  return GetNStrip(pl);
}

////////////////////////////////////////////////////////////////////

unsigned char PHMuoTrackAdcv1::GetStrip(int pl,int istrip) const{
  if(!CheckStripNum(pl,istrip)){return 0;} // both pl and istrip are checked
  return hit_strip[pl]+istrip;
}

//////////////////////////////////////////////////////////////////////

unsigned char PHMuoTrackAdcv1::GetStrip(int st,int gap,int cath,int istrip) const{
  int pl=cath+gap*2+st*6;
  return GetStrip(pl,istrip);
}

////////////////////////////////////////////////////////////////////////

unsigned short PHMuoTrackAdcv1::GetAdc(int pl,int istrip,int iadc) const{
  if(!CheckStripNum(pl,istrip)){return 0;} // both pl and istrip are checked
  if(!CheckAdcNum(iadc)){return 0;}

  int n_adc_hit_pl=0;
  for(int ipl=0; ipl<pl; ipl++){n_adc_hit_pl+=n_adc_hit[ipl];}

  int num=n_adc_hit_pl+istrip*N_ADC_SAMPLE+iadc;

  return raw_adc[num];
}

////////////////////////////////////////////////////////////////////////

unsigned short PHMuoTrackAdcv1::GetAdc(int st,int gap,int cath,
					int istrip,int iadc) const{
  int pl=cath+gap*2+st*6;
  return GetAdc(pl,istrip,iadc);
}

///////////////////////////////////////////////////////////////////////////
