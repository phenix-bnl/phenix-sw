#ifndef __MPCCLUSTERCONTAINER_H__
#define __MPCCLUSTERCONTAINER_H__

class mpcClusterContent;

#include <iosfwd>
#include "phool.h"
#include "PHObject.h"
 
/** (ABC) Container of MPC clusters.    

@ingroup interface

 */

class TClonesArray;

class mpcClusterContainer : public PHObject
{
public:

  virtual ~mpcClusterContainer(){}

  /// Add a new cluster (using default mpcClusterContentv# ctor).
  virtual mpcClusterContent* addCluster(unsigned int i) 
  { PHOOL_VIRTUAL_WARNING; return 0; }

  /** Add a new cluster (using copy ctor).
      c must be of compatible type, otherwise this will return 0.
  */
  virtual mpcClusterContent* addCluster(unsigned int i, 
					const mpcClusterContent& c) 
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual mpcClusterContent* addCluster(const mpcClusterContent &c);

  // the size you'll never exceed for this object.
  virtual unsigned int capacity(void) const 
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual mpcClusterContainer* clone(void) const 
  { PHOOL_VIRTUAL_WARNING; return 0; }
 
  virtual mpcClusterContainer* create(void) const 
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual mpcClusterContent* findCluster(int clusterid) const
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual mpcClusterContent* getCluster(unsigned int i) const 
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual void identify(std::ostream& os=std::cout) const
  { PHOOL_VIRTUAL_WARNING; }

  virtual int isValid() const
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual void print(std::ostream& os=std::cout) const;

  virtual bool removeCluster(unsigned int i) 
  { PHOOL_VIRTUAL_WARNING; return false; }

  virtual bool resize(unsigned int newsize) 
  { PHOOL_VIRTUAL_WARNING; return false; }

  virtual void Reset()
  { PHOOL_VIRTUAL_WARNING; } 

  // the actual size of the object.
  virtual unsigned int size(void) const 
  { PHOOL_VIRTUAL_WARNING; return 0; }

protected:
  virtual TClonesArray *GetTCArray() const {return 0;}

    /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
  mpcClusterContainer& operator=(const mpcClusterContainer&);

 private:

  ClassDef(mpcClusterContainer,0) // Array of mpcClusterContainer (ABC)
    
};

#endif
