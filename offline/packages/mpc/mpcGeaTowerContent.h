#ifndef __MPCGEATOWERCONTENT_H__
#define __MPCGEATOWERCONTENT_H__

#include <TObject.h>
#include <iostream>
#include "phool.h"

class mpcGeaTowerContent : public TObject
{
public:
  mpcGeaTowerContent();
  virtual ~mpcGeaTowerContent() {}

/*
  virtual mpcGeaTowerContent& operator=(const mpcGeaTowerContent &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }
  virtual mpcGeaTowerContent& operator+(const mpcGeaTowerContent &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }
  virtual mpcGeaTowerContent& operator+=(const mpcGeaTowerContent &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }
*/

  virtual Short_t get_ch() const       { PHOOL_VIRTUAL_WARNING; return -9999; }

  /// get geant track number of original primary
  virtual Int_t get_itorigin() const   { PHOOL_VIRTUAL_WARNING; return -9999; }
  /// get geant pid of original primary
  virtual Int_t get_idorigin() const   { PHOOL_VIRTUAL_WARNING; return -9999; }
  /// geant track number of incoming particle
  virtual Int_t get_itincoming() const { PHOOL_VIRTUAL_WARNING; return -9999; }
  /// geant pid of incoming particle
  virtual Int_t get_idincoming() const { PHOOL_VIRTUAL_WARNING; return -9999; }
  /// deposited energy from incoming particle
  virtual Float_t get_edep() const     { PHOOL_VIRTUAL_WARNING; return 0.; }
  /// percentage of total edep in tower from inc. ptcl.
  virtual Float_t get_fraction() const { PHOOL_VIRTUAL_WARNING; return 0.; }

  virtual void set_ch(const Short_t c)       { PHOOL_VIRTUAL_WARNING; }
  virtual void set_itorigin(const Int_t i)   { PHOOL_VIRTUAL_WARNING; }
  virtual void set_idorigin(const Int_t i)   { PHOOL_VIRTUAL_WARNING; }
  virtual void set_itincoming(const Int_t i) { PHOOL_VIRTUAL_WARNING; }
  virtual void set_idincoming(const Int_t i) { PHOOL_VIRTUAL_WARNING; }
  virtual void set_edep(const Float_t e)     { PHOOL_VIRTUAL_WARNING; }
  virtual void set_fraction(const Float_t f) { PHOOL_VIRTUAL_WARNING; }

  // the below are not needed except in studies of the showering process
  virtual Int_t get_track() const      { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual Int_t get_idpart() const     { PHOOL_VIRTUAL_WARNING; return -9999; }

  virtual void set_track(const Int_t t)      { PHOOL_VIRTUAL_WARNING; }
  virtual void set_idpart(const Int_t i)     { PHOOL_VIRTUAL_WARNING; }

  //virtual void Copy( const unsigned int iclus ) { PHOOL_VIRTUAL_WARNING; }
  virtual void print(std::ostream& = std::cout) const { PHOOL_VIRTUAL_WARNING; }

protected:
  //
  // this is the base class for single calibrated crystals
  //

private:

  ClassDef(mpcGeaTowerContent,1)
};

#endif /* __MPCGEATOWERCONTENT_H__ */

