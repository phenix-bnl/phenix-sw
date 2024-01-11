#ifndef __MPCRAWCONTENTV2_H__
#define __MPCRAWCONTENTV2_H__

#include <mpcRawContent.h>
#include <iostream>

class mpcRawContentV2 : public mpcRawContent
{
public:

  mpcRawContentV2();
  mpcRawContentV2(const mpcRawContent&);
  virtual ~mpcRawContentV2() {}

  mpcRawContent& operator=(const mpcRawContent &rhs);
  mpcRawContent& operator+(const mpcRawContent &rhs);
  mpcRawContent& operator+=(const mpcRawContent &rhs);

  short get_ch() const;//     { return ch; }
  short get_tdc() const    { return tdc; }
  short get_lopost() const { return lopost; }
  short get_lopre() const  { return lopre; }
  short get_hipost() const { return hipost; }
  short get_hipre() const  { return hipre; }

  void  set_ch(const int c)      { ch = c; }
  void  set_tdc(const int t)     { tdc = t; }
  void  set_lopost(const int lp) { lopost = lp; }
  void  set_lopre(const int lp)  { lopre = lp; }
  void  set_hipost(const int hp) { hipost = hp; }
  void  set_hipre(const int hp)  { hipre = hp; }

  void  set(const int c, const int t, const int lpost, const int lpre,
            const int hpost, const int hpre) {
    ch = c;
    tdc = t;
    lopost = lpost;
    lopre = lpre;
    hipost = hpost;
    hipre = hpre;
  }

  virtual void print(std::ostream&);

protected:
 
  short ch;		// fem channel number (from 0 to 287)
  short tdc;		// tdc
  short lopost;		// low gain adc post sample
  short lopre;		// low gain adc pre sample
  short hipost;		// high gain adc post sample
  short hipre;		// high gain adc pre sample

  ClassDef(mpcRawContentV2,1)
};

#endif /* __MPCRAWCONTENTV2_H__ */

