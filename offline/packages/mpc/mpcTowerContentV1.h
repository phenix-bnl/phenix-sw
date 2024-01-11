#ifndef __MPCTOWERCONTENTV1_H__
#define __MPCTOWERCONTENTV1_H__

#include "mpcTowerContent.h"
#include <iostream>

class mpcTowerContentV1 : public mpcTowerContent
{
public:

  mpcTowerContentV1();
  mpcTowerContentV1(mpcTowerContent&);
  virtual ~mpcTowerContentV1() {}

  mpcTowerContent& operator=(mpcTowerContent &rhs);
  mpcTowerContent& operator+(mpcTowerContent &rhs);
  mpcTowerContent& operator+=(mpcTowerContent &rhs);

  short get_ch() const     { return ch; }
  float get_tof() const    { return tof; }
  float get_energy(const int noise_alg = 1) const { return energy; }
  short get_type() const     { return 1; } //V1

  void  set_ch(const int c)      { ch = c; }
  void  set_tof(const float t)   { tof = t; }
  void  set_energy(const float e){ energy = e; }

  void  set(const int c, const float t, const float e) {
    ch = c;
    tof = t;
    energy = e;
  }

  void print(std::ostream&) const;

protected:
 
  short ch;		// fem channel number (from 0 to 575)
  float tof;		// tof (ns)
  float energy;		// best energy (GeV)

  ClassDef(mpcTowerContentV1,1)
};

#endif /* __MPCTOWERCONTENTV1_H__ */

