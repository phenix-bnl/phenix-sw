#ifndef __MPCTOWERCONTENT_H__
#define __MPCTOWERCONTENT_H__

#include <TObject.h>
#include <iostream>
#include "phool.h"

class mpcTowerContent : public TObject
{
public:
  mpcTowerContent();
  virtual ~mpcTowerContent() {}

  virtual mpcTowerContent& operator=(mpcTowerContent &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }
  virtual mpcTowerContent& operator+(mpcTowerContent &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }
  virtual mpcTowerContent& operator+=(mpcTowerContent &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  virtual Short_t get_ch() const     { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual Float_t get_tof() const    { PHOOL_VIRTUAL_WARNING; return -9999.; }
  virtual Float_t get_energy(const int noise_alg=1) const { PHOOL_VIRTUAL_WARNING; return -9999.; }
  virtual Short_t get_type() const     { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual Float_t get_noise() const { PHOOL_VIRTUAL_WARNING; return -9999; }

  virtual void set_ch(const Int_t c)       { PHOOL_VIRTUAL_WARNING; }
  virtual void set_tof(const Float_t t)    { PHOOL_VIRTUAL_WARNING; }
  virtual void set_energy(const Float_t e) { PHOOL_VIRTUAL_WARNING; }
  virtual void set_noise(const Float_t e) { PHOOL_VIRTUAL_WARNING; }

  virtual void set(const Int_t c, const Float_t t, const Float_t e) { PHOOL_VIRTUAL_WARNING; }

  //virtual void Copy( const unsigned int iclus ) { PHOOL_VIRTUAL_WARNING; }
  virtual void print(std::ostream& = std::cout) const { PHOOL_VIRTUAL_WARNING; }

protected:
  //
  // this is the base class for single calibrated crystals
  //

private:

  ClassDef(mpcTowerContent,1)
};

#endif /* __MPCTOWERCONTENT_H__ */

