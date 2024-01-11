#ifndef __MPCTOWERCONTAINER_H__
#define __MPCTOWERCONTAINER_H__

#include <iosfwd>
#include "phool.h"
#include "PHObject.h"
 
class mpcTowerContent;

/** (ABC) Container of Calibrated MPC towers.    

@ingroup interface

 */

class TClonesArray;

class mpcTowerContainer : public PHObject
{
public:

  mpcTowerContainer();

  virtual ~mpcTowerContainer(){}

  virtual mpcTowerContainer& operator=(const mpcTowerContainer &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  virtual mpcTowerContainer& operator+(const mpcTowerContainer &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  virtual mpcTowerContainer& operator+=(const mpcTowerContainer &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  virtual mpcTowerContainer& operator*=(const float scale)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  // Scale the North or South energy
  virtual void scale(const Float_t escale, const Int_t arm);

  /** Set the AMU cell addresses
  */
  virtual void set_amu(const int tdc_amu, const int pre_amu, const int post_amu)
  { PHOOL_VIRTUAL_WARNING; }

  // Get the TDC AMU cell address
  virtual short get_tdc_amu() const
  { PHOOL_VIRTUAL_WARNING; return -999; }

  // Get the Pre-Sample AMU cell address
  virtual short get_pre_amu() const
  { PHOOL_VIRTUAL_WARNING; return -999; }

  // Get the Post-Sample AMU cell address
  virtual short get_post_amu() const
  { PHOOL_VIRTUAL_WARNING; return -999; }

  virtual short get_type() const
  { PHOOL_VIRTUAL_WARNING; return -999; }

  // Get the energy sum of all towers
  virtual float get_esum() const
  { PHOOL_VIRTUAL_WARNING; return -999.; }

  // Get the un-grayed TDC AMU cell address
  virtual short get_ungray_tdc_amu() const;

  // Get the un-grayed Pre-Sample AMU cell address
  virtual short get_ungray_pre_amu() const;

  // Get the un-grayed Post-Sample AMU cell address
  virtual short get_ungray_post_amu() const;

  virtual int findTower(const int ifeech);

  /** Add a new tower (using copy ctor).
      c must be of compatible type, otherwise this will return 0.
  */
  virtual mpcTowerContent* addTower(mpcTowerContent &c)
  { PHOOL_VIRTUAL_WARNING; return NULL; }

  virtual mpcTowerContent* getTower(unsigned int i) const 
  { PHOOL_VIRTUAL_WARNING; return 0; }

  // the actual size of the object.
  virtual unsigned int size(void) const 
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual int isValid() const
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual void Reset()
  { PHOOL_VIRTUAL_WARNING; } 

  virtual void identify(std::ostream& os=std::cout) const
  { PHOOL_VIRTUAL_WARNING; }

  virtual void print(std::ostream& os=std::cout) const
  { PHOOL_VIRTUAL_WARNING; }

  virtual TClonesArray *GetArray() const { PHOOL_VIRTUAL_WARNING; return 0;}
protected:

    /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
//  mpcTowerContainer& operator=(const mpcTowerContainer&)
//  { PHOOL_VIRTUAL_WARNING; }

  virtual unsigned long get_gray(unsigned long n, int is) const;

  //int mpctoweridmap[576];	//! map of index to towerids

private:

  ClassDef(mpcTowerContainer,1) // Array of mpcTowerContainer (ABC)
    
};

#endif
