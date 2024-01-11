#ifndef __MPCGEATOWERCONTENTV1_H__
#define __MPCGEATOWERCONTENTV1_H__

#include "mpcGeaTowerContent.h"
#include <iostream>

class mpcGeaTowerContentV1 : public mpcGeaTowerContent
{
public:

  mpcGeaTowerContentV1();
  mpcGeaTowerContentV1(const mpcGeaTowerContent&);
  virtual ~mpcGeaTowerContentV1() {}

/*
  mpcGeaTowerContent& operator=(const mpcGeaTowerContent &rhs);
  mpcGeaTowerContent& operator+(const mpcGeaTowerContent &rhs);
  mpcGeaTowerContent& operator+=(const mpcGeaTowerContent &rhs);
*/

  Short_t get_ch() const       { return ch; }
  Int_t get_itorigin() const   { return itorigin; }
  Int_t get_idorigin() const   { return idorigin; }
  Int_t get_itincoming() const { return itincoming; }
  Int_t get_idincoming() const { return idincoming; }
  Float_t get_edep() const     { return edep; }
  Float_t get_fraction() const { return fraction; }
  
  void set_ch(const Short_t c)       { ch = c; }
  void set_itorigin(const Int_t i)   { itorigin = i; }
  void set_idorigin(const Int_t i)   { idorigin = i; }
  void set_itincoming(const Int_t i) { itincoming = i; }
  void set_idincoming(const Int_t i) { idincoming = i; }
  void set_edep(const Float_t e)     { edep = e; }
  void set_fraction(const Float_t f) { fraction = f; }

  void print(std::ostream&) const;

protected:
 
  Short_t ch;		// fem channel number (from 0 to 575)
  Int_t itorigin;	// geant track number of original primary
  Int_t idorigin;	// geant pid of original primary
  Int_t itincoming;	// geant track number of incoming particle
  Int_t idincoming;	// geant pid of incoming particle
  Float_t edep;		// deposited energy from incoming particle
  Float_t fraction;	// percentage of total edep in tower from inc. ptcl.

  ClassDef(mpcGeaTowerContentV1,1)
};

#endif /* __MPCGEATOWERCONTENTV1_H__ */

