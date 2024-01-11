#ifndef __MVDRPHIZOUT_h
#define __MVDRPHIZOUT_h

#include "PHObject.h"
#include <iostream>

class MvdSnglRPhiZv1;

class MvdRPhiZOut: public PHObject
{
 public:

  virtual ~MvdRPhiZOut() {}

  virtual void Reset();

  virtual void identify(std::ostream& os = std::cout) const;
  virtual int isValid() const;

  virtual short int AddMvdRPhiZ(MvdSnglRPhiZv1* newdndeta, unsigned int thishit) {return -999;}
  virtual unsigned int get_MvdNRPhiZ() const {return 0;}
  virtual void set_MvdNRPhiZ(unsigned int ndndeta) {return;}
  virtual unsigned int set_TClonesArraysize(unsigned int nhits) {return 0;}

  virtual unsigned short get_r(unsigned int ihit) const {return 9999;}
  virtual unsigned short get_phi(unsigned int ihit) const {return 9999;}
  virtual unsigned short get_z(unsigned int ihit) const {return 9999;}
  virtual unsigned short get_adc(unsigned int ihit) const {return 9999;}

  // return floats for the integer position location
  virtual float getR(unsigned int iHit) const;
  virtual float getPhi(unsigned int iHit) const;
  virtual float getZ(unsigned int iHit) const;

  ClassDef(MvdRPhiZOut,1)

}; 

#endif /*__MVDRPHIZOUT_h*/
