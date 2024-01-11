#ifndef __MPCGEACLUSTERCONTENTV1_H__
#define __MPCGEACLUSTERCONTENTV1_H__

#include "mpcGeaClusterContent.h"
#include <iostream>

class mpcGeaClusterContentV1 : public mpcGeaClusterContent
{
public:

  mpcGeaClusterContentV1();
  mpcGeaClusterContentV1(const mpcGeaClusterContent&);
  virtual ~mpcGeaClusterContentV1() {}

/*
  mpcGeaClusterContent& operator=(const mpcGeaClusterContent &rhs);
  mpcGeaClusterContent& operator+(const mpcGeaClusterContent &rhs);
  mpcGeaClusterContent& operator+=(const mpcGeaClusterContent &rhs);
*/

  Short_t get_ch() const       { return ch; }
  Int_t get_itorigin() const   { return itorigin; }
  Int_t get_idorigin() const   { return idorigin; }
  Int_t get_itincoming() const { return itincoming; }
  Int_t get_idincoming() const { return idincoming; }
  Float_t get_edep() const     { return edep; }
  Float_t get_fraction() const { return fraction; }
  Float_t get_fraction_tr() const { return fraction_tr; }

  //pythia
  Int_t get_type() const { return fType; }
  Int_t get_id() const { return fID; }
  Float_t get_py_energy() const { return fEnergy; }
  Int_t get_parent_type() const { return fParentType; }
  Int_t get_parent_id() const { return fParentID; }
  
  void set_ch(const Short_t c)       { ch = c; }
  void set_itorigin(const Int_t i)   { itorigin = i; }
  void set_idorigin(const Int_t i)   { idorigin = i; }
  void set_itincoming(const Int_t i) { itincoming = i; }
  void set_idincoming(const Int_t i) { idincoming = i; }
  void set_edep(const Float_t e)     { edep = e; }
  void set_fraction(const Float_t f) { fraction = f; }
  void set_fraction_tr(const Float_t f) { fraction_tr = f; }

  
  void set_type(const int type){ fType=type; }
  void set_id(const int id){ fID=id; }
  void set_py_energy(const float en){ fEnergy=en; }
  void set_parent_type(const int type){ fParentType=type; }
  void set_parent_id(const int id){ fParentID=id; }

  void print(std::ostream&) const;

protected:
 
  Short_t ch;		// fem channel number (from 0 to 575)
  Int_t itorigin;	// geant track number of original primary
  Int_t idorigin;	// geant pid of original primary
  Int_t itincoming;	// geant track number of incoming particle
  Int_t idincoming;	// geant pid of incoming particle
  Float_t edep;		// deposited energy from incoming particle
  Float_t fraction;	// percentage of total cluster edep from inc. ptcl.
  Float_t fraction_tr;	// percentage of total edep in cluster from inc. ptcl.

  //pythia info
  Int_t fType;
  Int_t fID;
  Float_t fEnergy;
  Int_t fParentType;
  Int_t fParentID;
  
  
  

  ClassDef(mpcGeaClusterContentV1,1)
};

#endif /* __MPCGEACLUSTERCONTENTV1_H__ */

