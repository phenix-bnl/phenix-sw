#ifndef __MPCRAWCONTENTV3_H__
#define __MPCRAWCONTENTV3_H__

#include <mpcRawContent.h>
#include <iostream>

//
// New Electronics Data
//
class mpcRawContentV3 : public mpcRawContent
{
public:

  mpcRawContentV3();
  mpcRawContentV3(const mpcRawContent&);
  virtual ~mpcRawContentV3() {}

  mpcRawContent& operator=(const mpcRawContent &rhs);
  mpcRawContent& operator+(const mpcRawContent &rhs);
  mpcRawContent& operator+=(const mpcRawContent &rhs);

  Short_t get_ch() const     { return ch; }
  Short_t get_tdc() const    { return tdc; }	// tdc is sample*360.
  Float_t get_sample() const    { return tdc/360.; }
  Float_t get_time() const    { return (tdc/360.)*17.762; }
  Float_t get_adc() const    { return adc; }
  Short_t get_quality() const { return fquality; }

  //this contains information about the fit
  //if the amplitude is too small to do a fit
  //if the amplitude comes back negative, or some other fit parameter is bad
  void set_quality(const Short_t q) { fquality=q; }


  void  set_ch(const Int_t c)      { ch = c; }
  void  set_tdc(const Int_t t)     { tdc = t; }
  void  set_adc(const Float_t a)   { adc = a; }


  void  set(const Int_t c, const Int_t t, const Float_t a) {
    ch = c;
    tdc = t;
    adc = a;
  }

  virtual void print(std::ostream&);


protected:
 
  Short_t ch;		// fem channel number (from 0 to 576)
  Short_t tdc;		// tdc (4095==overflow)
  Float_t adc;		// adc pre sample

  Short_t fquality;

  ClassDef(mpcRawContentV3,1)
};

#endif /* __MPCRAWCONTENTV2_H__ */

