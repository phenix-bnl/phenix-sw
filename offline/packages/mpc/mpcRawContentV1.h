#ifndef __MPCRAWCONTENTV1_H__
#define __MPCRAWCONTENTV1_H__

#include <mpcRawContent.h>
#include <iostream>

class mpcRawContentV1 : public mpcRawContent
{
public:

  mpcRawContentV1();
  mpcRawContentV1(const mpcRawContent&);
  virtual ~mpcRawContentV1() {}

  mpcRawContent& operator=(const mpcRawContent &rhs);
  mpcRawContent& operator+(const mpcRawContent &rhs);
  mpcRawContent& operator+=(const mpcRawContent &rhs);

  short get_ch() const;
  short get_tdc() const    { return tdc; }
  short get_lopost() const { return lopost; }
  short get_lopre() const  { return lopre; }
  short get_hipost() const { return 0; }
  short get_hipre() const  { return 0; }

  void  set_ch(const int c)      { ch = c; }
  void  set_tdc(const int t)     { tdc = t; }
  void  set_lopost(const int lp) { lopost = lp; }
  void  set_lopre(const int lp)  { lopre = lp; }

  void  set(const int c, const int t, const int lpost, const int lpre) {
    ch = c;
    tdc = t;
    lopost = lpost;
    lopre = lpre;
  }

  virtual void print(std::ostream&);

protected:
 
  short ch;		// fem channel number (from 0 to 287)
  short tdc;		// tdc
  short lopost;		// low gain adc post sample
  short lopre;		// low gain adc pre sample

  ClassDef(mpcRawContentV1,1)
};

#endif /* __MPCRAWCONTENTV1_H__ */

