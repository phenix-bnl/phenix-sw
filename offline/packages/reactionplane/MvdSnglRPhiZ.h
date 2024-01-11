#ifndef __MVDSNGLRPHIZ_h
#define __MVDSNGLRPHIZ_h

#include "PHObject.h"
#include <iostream>

class MvdSnglRPhiZ: public PHObject
{
 public:
  virtual ~MvdSnglRPhiZ() {}

  virtual void identify(std::ostream& os = std::cout) const;
  virtual unsigned short get_r() const { return 9999; }
  virtual unsigned short get_phi() const { return 9999; }
  virtual unsigned short get_z() const { return 9999; }
  virtual unsigned short get_adc() const { return 9999; }
  virtual float get_rfloat() const { return -9999.; }
  virtual float get_phifloat() const { return -9999.; }
  virtual float get_zfloat() const { return -9999.; }

protected:

  ClassDef(MvdSnglRPhiZ,1)

};

#endif /* __MVDSNGLRPHIZ_h */
