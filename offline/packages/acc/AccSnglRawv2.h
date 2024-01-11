
#ifndef __ACCSNGLRAWV2_H_
#define __ACCSNGLRAWV2_H_

#include "AccSnglRaw.h"

class AccSnglRawv2 : public AccSnglRaw
{

 public:
  AccSnglRawv2();
  AccSnglRawv2(AccSnglRawv2* track);
  virtual ~AccSnglRawv2() {}  
  void identify(std::ostream& os=std::cout) const;

  // set the values in the SnglRaw
  void set_boxid(const int val)                {boxid  = val; return;}
  void set_adc(const int i, const int val)     {adc[i] = val; return;}
  void set_tdc(const int i, const int val)     {tdc[i] = val; return;}
  void set_adcpost(const int i, const int val) {adc_post[i] = val; return;}
  void set_adcpre(const int i, const int val)  {return;}

  // get the values from the SnglRaw
  int get_boxid() const { return boxid;}
  int get_adc(const int i) const { return adc[i]; }
  int get_tdc(const int i) const { return tdc[i]; }
  int get_adcpost(const int i) const { return adc_post[i]; }
  int get_adcpre(const int i) const { return (adc_post[i]-adc[i]); }

 protected:
  short int boxid;  // box id
  short int adc[2]; // adc 
  short int tdc[2]; // tdc
  short int adc_post[2];  // adc post sample

  ClassDef(AccSnglRawv2,1)
};

#endif
