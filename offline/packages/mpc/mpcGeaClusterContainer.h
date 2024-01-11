#ifndef __MPCGEACLUSTERCONTAINER_H__
#define __MPCGEACLUSTERCONTAINER_H__

#include <iosfwd>
#include <phool.h>
#include <PHObject.h>
 
class mpcGeaClusterContent;

/** (ABC) Container of MPC cluster geant info.    

@ingroup interface

 */

class TClonesArray;

class mpcGeaClusterContainer : public PHObject
{
public:

  mpcGeaClusterContainer();

  virtual ~mpcGeaClusterContainer(){}

/*
  virtual mpcGeaClusterContainer& operator=(const mpcGeaClusterContainer &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  virtual mpcGeaClusterContainer& operator+(const mpcGeaClusterContainer &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  virtual mpcGeaClusterContainer& operator+=(const mpcGeaClusterContainer &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }
*/

  /// Find McTrack of Original Primary which left largest energy in ch
  virtual mpcGeaClusterContent *findPrimary(const int ch);
  virtual int findPrimaryIndex(const int ch);
  virtual int findGeaClusIndex(const int it, int opt = 0); //opt=0 --> use it=itorigin
                                                           //opt=1 --> use it=itincoming
  
  // Get the energy sum of all towers
/*
  virtual float get_esum() const
  { PHOOL_VIRTUAL_WARNING; return -999.; }
*/

  /** Add a new tower (using copy ctor).
      c must be of compatible type, otherwise this will return 0.
  */
  virtual mpcGeaClusterContent* addCluster(const mpcGeaClusterContent &c)
  { PHOOL_VIRTUAL_WARNING; return NULL; }

  virtual mpcGeaClusterContent* getCluster(unsigned int i) const 
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
//  mpcGeaClusterContainer& operator=(const mpcGeaClusterContainer&)
//  { PHOOL_VIRTUAL_WARNING; }

private:

  ClassDef(mpcGeaClusterContainer,1) // Array of mpcGeaClusterContainer (ABC)
    
};

#endif	// __MPCGEACLUSTERCONTAINER_H__
