#ifndef __PHMUOTRACKADC__
#define __PHMUOTRACKADC__

#include <TObject.h>

#include <vector>

class PHMuoTrackAdc : public TObject{
public:
  enum{
    MAX_CATHODE_PLANE=16,
    MAX_ADC_HIT=640,
    N_ADC_SAMPLE=4
  };

  PHMuoTrackAdc(void){;}
  virtual ~PHMuoTrackAdc(void){;}
  virtual void Reset(void)=0;

  virtual void SetTrackUid(unsigned int uid)=0;
  virtual unsigned int GetTrackUid(void)=0;
  virtual int SetAdc(std::vector<unsigned char> strip_array,
		     std::vector<std::vector<unsigned short> > adc_array)=0;
  virtual void GetAdc(std::vector<unsigned char> &strip_array,
		      std::vector<std::vector<unsigned short> > &adc_array) const =0;

  virtual unsigned char GetNStrip(int pl) const =0;
  virtual unsigned char GetNStrip(int st,int gap,int cath) const =0;
  virtual unsigned char GetStrip(int pl,int istrip) const =0;
  virtual unsigned char GetStrip(int st,int gap,int cath,int istrip) const =0;
  virtual unsigned short GetAdc(int pl,int istrip,int iadc) const =0;
  virtual unsigned short GetAdc(int st,int gap,int cath,int istrip,int iadc) const =0;

  virtual bool CheckPlaneNum(int pl) const;
  virtual bool CheckStripNum(int pl,int istrip) const;
  virtual bool CheckAdcNum(int iadc) const;

protected:
  ClassDef(PHMuoTrackAdc,1)
};

#endif /* __PHMUOTRACKADC__ */
