#ifndef __PHMUOTRACKADCV1__
#define __PHMUOTRACKADCV1__

#include "PHMuoTrackAdc.h"

#include <vector>

class PHMuoTrackAdcv1 : public PHMuoTrackAdc{
public:
  PHMuoTrackAdcv1(void);
  virtual ~PHMuoTrackAdcv1(void);
  virtual void Reset(void);

  virtual void SetTrackUid(unsigned int uid){track_uid=uid;}
  virtual unsigned int GetTrackUid(void){return track_uid;}
  virtual int SetAdc(std::vector<unsigned char> strip_array,
		     std::vector<std::vector<unsigned short> > adc_array);
  virtual void GetAdc(std::vector<unsigned char> &strip_array,
		      std::vector<std::vector<unsigned short> > &adc_array) const;

  virtual unsigned char GetNStrip(int pl) const;
  virtual unsigned char GetNStrip(int st,int gap,int cath) const;
  virtual unsigned char GetStrip(int pl,int istrip) const;
  virtual unsigned char GetStrip(int st,int gap,int cath,int istrip) const;
  virtual unsigned short GetAdc(int pl,int istrip,int iadc) const;
  virtual unsigned short GetAdc(int st,int gap,int cath,int istrip,int iadc) const;

protected:
  unsigned int track_uid;
  int n_adc_hit_total;
  unsigned char n_adc_hit[MAX_CATHODE_PLANE];
  unsigned char hit_strip[MAX_CATHODE_PLANE];
  unsigned short *raw_adc; //[n_adc_hit_total]

  ClassDef(PHMuoTrackAdcv1,1)
};

#endif /* __PHMUOTRACKADCV1__ */
