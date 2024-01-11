#ifndef __MVDSNGLRPHIZV1_h
#define __MVDSNGLRPHIZV1_h

#include "MvdSnglRPhiZ.h"
#include <iostream>

#define RMASK   0x1F
#define PHIMASK 0xFF
#define ZMASK   0x01

#define RSHIFT   0
#define PHISHIFT 5
#define ZSHIFT   13

class MvdSnglRPhiZv1: public MvdSnglRPhiZ
{
 public:
  MvdSnglRPhiZv1(): adc(0), rphiz(0) {}
  MvdSnglRPhiZv1(MvdSnglRPhiZv1* newrphiz);
  virtual ~MvdSnglRPhiZv1() {}

  void identify(std::ostream& os = std::cout) const;
  unsigned short get_r() const { return(rphiz >> RSHIFT) & RMASK; }
  unsigned short get_phi() const { return (rphiz >> PHISHIFT) & PHIMASK; }
  unsigned short get_z() const { return (rphiz >> ZSHIFT) & ZMASK; }
  unsigned short get_adc() const { return (unsigned short)adc; }
  float get_rfloat() const { return -9999.; }
  float get_phifloat() const { return -9999.; }
  float get_zfloat() const { return -9999.; }

  short set_r(const unsigned short r);
  short set_phi(const unsigned short phi);
  short set_z(const unsigned short z);
  short set_adc(const unsigned short ival) {adc = ival; return 0;}
  void clear(){ rphiz = 0; adc = 0; }

protected:
  bool checkR(const unsigned short r){
    if(r>20) return false;
    else     return true;
  }
  bool checkPhi(const unsigned short phi){
    if(phi>143) return false;
    else        return true;
  }
  bool checkZ(const unsigned short z){
    if(z>1) return false;
    else    return true;
  }
    
  unsigned char adc;     /* 8 bit ADC */
  unsigned short rphiz;  /* low 5 bits: r */
                         /* next 8 bits phi */
                         /* next 1 bit z */

  ClassDef(MvdSnglRPhiZv1,1)

};

#endif /* __MVDSNGLRPHIZV1_h */
