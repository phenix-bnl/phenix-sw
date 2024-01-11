
#ifndef __ACCSNGLRAWV1_H_
#define __ACCSNGLRAWV1_H_

#include "AccSnglRaw.h"

class AccSnglRawv1 : public AccSnglRaw
{

 public:
  AccSnglRawv1();
  AccSnglRawv1(AccSnglRawv1* track);
  virtual ~AccSnglRawv1() {}  

  // set the values in the SnglRaw
  void set_boxid(const int val)                {boxid  = val; return;}
  void set_adc(const int i, const int val)     {adc[i] = val; return;}
  void set_tdc(const int i, const int val)     {tdc[i] = val; return;}
  void set_adcpost(const int i, const int val) {adc_post[i] = val; return;}
  void set_adcpre(const int i, const int val)  {adc_pre[i] = val; return;}

  // get the values from the SnglRaw
  int get_boxid() const { return boxid;}
  int get_adc(const int i) const { return adc[i]; }
  int get_tdc(const int i) const { return tdc[i]; }
  int get_adcpost(const int i) const { return adc_post[i]; }
  int get_adcpre(const int i) const { return adc_pre[i]; }

 protected:
  int boxid;  // box id
  int adc[2]; // adc 
  int tdc[2]; // tdc
  int adc_post[2]; // adc post sample
  int adc_pre[2];  // adc pre sample

  ClassDef(AccSnglRawv1,1)
};

#endif
