#ifndef __MPCRAWCONTAINER_H__
#define __MPCRAWCONTAINER_H__

#include <iosfwd>
#include "phool.h"
#include "PHObject.h"
 
class mpcRawContent;

/** (ABC) Container of MPC towers.    

@ingroup interface

 */

class TClonesArray;

class mpcRawContainer : public PHObject
{
public:

  mpcRawContainer();

  virtual ~mpcRawContainer(){}

  virtual mpcRawContainer& operator=(const mpcRawContainer &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  virtual mpcRawContainer& operator+(const mpcRawContainer &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  virtual mpcRawContainer& operator+=(const mpcRawContainer &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

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
 
  // Get the un-grayed TDC AMU cell address
  virtual short get_ungray_tdc_amu() const;

  // Get the un-grayed Pre-Sample AMU cell address
  virtual short get_ungray_pre_amu() const;

  // Get the un-grayed Post-Sample AMU cell address
  virtual short get_ungray_post_amu() const;
 
  /** Add a new tower (using copy ctor).
      c must be of compatible type, otherwise this will return 0.
  */
  virtual mpcRawContent* addTower(const mpcRawContent &c)
  { PHOOL_VIRTUAL_WARNING; return NULL; }

  virtual mpcRawContent* getTower(unsigned int i) const 
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual int findTower(const int feech) const;

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
//  virtual TClonesArray *GetArray() const { PHOOL_VIRTUAL_WARNING; return 0;}

    /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
//  mpcRawContainer& operator=(const mpcRawContainer&)
//  { PHOOL_VIRTUAL_WARNING; }

  virtual unsigned long get_gray(unsigned long n, int is) const;

private:

  ClassDef(mpcRawContainer,1) // Array of mpcRawContainer (ABC)
    
};

#endif
