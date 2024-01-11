#ifndef __MPCGEATOWERCONTAINER_H__
#define __MPCGEATOWERCONTAINER_H__

#include <iosfwd>
#include "phool.h"
#include "PHObject.h"
 
class mpcGeaTowerContent;

/** (ABC) Container of Calibrated MPC towers.    

@ingroup interface

 */

class TClonesArray;

class mpcGeaTowerContainer : public PHObject
{
public:

  mpcGeaTowerContainer();

  virtual ~mpcGeaTowerContainer(){}

/*
  virtual mpcGeaTowerContainer& operator=(const mpcGeaTowerContainer &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  virtual mpcGeaTowerContainer& operator+(const mpcGeaTowerContainer &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  virtual mpcGeaTowerContainer& operator+=(const mpcGeaTowerContainer &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }
*/

  /// Find McTrack of Original Primary which left largest energy in ch
  virtual mpcGeaTowerContent *findPrimary(const int ch);

  // Get the energy sum of all towers
/*
  virtual float get_esum() const
  { PHOOL_VIRTUAL_WARNING; return -999.; }
*/

  /** Add a new tower (using copy ctor).
      c must be of compatible type, otherwise this will return 0.
  */
  virtual mpcGeaTowerContent* addTower(const mpcGeaTowerContent &c)
  { PHOOL_VIRTUAL_WARNING; return NULL; }

  virtual mpcGeaTowerContent* getTower(unsigned int i) const 
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
//  mpcGeaTowerContainer& operator=(const mpcGeaTowerContainer&)
//  { PHOOL_VIRTUAL_WARNING; }

private:

  ClassDef(mpcGeaTowerContainer,1) // Array of mpcGeaTowerContainer (ABC)
    
};

#endif	// __MPCGEATOWERCONTAINER_H__
