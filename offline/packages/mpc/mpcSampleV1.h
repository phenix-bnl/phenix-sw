#ifndef __MPCSAMPLEV1_H__
#define __MPCSAMPLEV1_H__

#include <mpcSample.h>
#include <iostream>

class mpcSampleV1 : public mpcSample
{
public:

  mpcSampleV1();
  mpcSampleV1(const mpcSample&);
  virtual ~mpcSampleV1() {}

  Short_t get_ch() const     { return ch; }
  Short_t get_adc() const    { return adc; }
  Char_t  get_sample() const { return sample; }

  void  set_ch(const Short_t c)     { ch = c; }
  void  set_adc(const Short_t a)     { adc = a; }
  void  set_sample(const Char_t s)   { sample = s; }

  void  set(const short c, const short a, const char s)
  {
    ch = c;
    adc = a;
    sample = s;
  }

  void print(std::ostream&);

protected:
 
  Char_t  sample;	// sample number (0-11)
  Short_t ch;		// fem channel number (from 0 to 576)
  Short_t adc;		// 12 bit adc

private:
  ClassDef(mpcSampleV1,1)
};

#endif /* __MPCSAMPLEV1_H__ */

