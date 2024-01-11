#ifndef __MPCSIMTOWERCONTENTV1_H__
#define __MPCSIMTOWERCONTENTV1_H__

#include <mpcTowerContent.h>
#include <iostream>

class mpcSimTowerContentV1 : public mpcTowerContent
{
public:

  mpcSimTowerContentV1();
  mpcSimTowerContentV1(mpcTowerContent&);
  virtual ~mpcSimTowerContentV1() {}

  mpcTowerContent& operator=(mpcTowerContent &rhs);
  mpcTowerContent& operator+(mpcTowerContent &rhs);
  mpcTowerContent& operator+=(mpcTowerContent &rhs);

  Short_t get_ch() const  { return ch; }
  Float_t get_tof() const   { return tof; }
  Float_t get_energy(const int noise_alg = 1) const;
  short get_type() const     { return 11; } //10 + 1 (V1)

  void  set_ch(const Int_t c)      { ch = c; }
  void  set_tof(const Float_t t)   { tof = t; }
  void  set_energy(const Float_t e) {
    energy = e;
    energy_noise = -9999.;
  }

  void  set(const int c, const float t, const float e) {
    ch = c;
    tof = t;
    energy = e;
  }

  void print(std::ostream&) const;

  float get_noise() const { return energy_noise; }
  void set_noise(float noise) { energy_noise = noise; }

protected:
 
  Short_t ch;		// fem channel number (from 0 to 575)
  Float_t tof;		// tof (ns)
  Float_t energy;		// best energy (GeV)

  mutable Float_t energy_noise;	//! noise to add to energy

  /// Set the noise. One can put in different algorithms here
  void get_energy_noise() const;

  ClassDef(mpcSimTowerContentV1,2)
};

#endif /* __MPCSIMTOWERCONTENTV1_H__ */

