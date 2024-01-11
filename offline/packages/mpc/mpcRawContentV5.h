#ifndef __MPCRAWCONTENTV5_H__
#define __MPCRAWCONTENTV5_H__

#include <mpcRawContent.h>
#include <iostream>

//
// New Electronics Data
//
class mpcRawContentV5 : public mpcRawContent
{
public:

  mpcRawContentV5();
  mpcRawContentV5(const mpcRawContent&);
  virtual ~mpcRawContentV5() {}

  mpcRawContent& operator=(const mpcRawContent &rhs);
  mpcRawContent& operator+(const mpcRawContent &rhs);
  mpcRawContent& operator+=(const mpcRawContent &rhs);

  Short_t get_ch() const     { return ch; }
  Short_t get_tdc() const    { return tdc; }	// tdc is sample*360.
  Short_t get_tdc1() const    { return tdc1; }	
  Short_t get_tdc2() const    { return tdc2; }	
  Float_t get_sample() const    { return tdc/360.; }
  Float_t get_time() const    { return (tdc/360.)*17.762; }
  Float_t get_time1() const    { return (tdc1/360.)*17.762; }
  Float_t get_time2() const    { return (tdc2/360.)*17.762; }
  Float_t get_adc() const    { return adc; }
  Float_t get_adc1() const    { return adc1; }
  Float_t get_adc2() const    { return adc2; }
  Float_t get_ZSM() const    { return zsm; }
  Short_t get_quality() const { return (Short_t) fquality; }
  Float_t get_fquality() const { return fquality; }

  //this contains information about the fit
  //if the amplitude is too small to do a fit
  //if the amplitude comes back negative, or some other fit parameter is bad
  void set_quality(const Float_t q) { fquality=q; }


  void  set_ch(const Int_t c)      { ch = c; }
  void  set_tdc(const Int_t t)     { tdc = t; }
  void  set_tdc1(const Int_t t)     { tdc1 = t; }
  void  set_tdc2(const Int_t t)     { tdc2 = t; }
  void  set_adc(const Float_t a)   { adc = a; }
  void  set_adc1(const Float_t a)   { adc1 = a; }
  void  set_adc2(const Float_t a)   { adc2 = a; }
  void  set_ZSM(const Float_t a)   { zsm = a; }

  void  set(const Int_t c, const Int_t t, const Float_t a) {
    ch = c;
    tdc = t;
    adc = a;
  }

  virtual void print(std::ostream&);


protected:
 
  Short_t ch;		// fem channel number (from 0 to 576)
  Short_t tdc;		// tdc (4095==overflow)
  Short_t tdc1;		// tdc1 first pulse tdc
  Short_t tdc2;		// tdc2 second pulse tdc
  Float_t adc;		// adc pre sample
  Float_t adc1;		// first pulse adc
  Float_t adc2;		// second pulse adc
  Float_t zsm;          // zero suppression metric

  Float_t fquality;

  ClassDef(mpcRawContentV5,1)
};

#endif /* __MPCRAWCONTENTV5_H__ */

