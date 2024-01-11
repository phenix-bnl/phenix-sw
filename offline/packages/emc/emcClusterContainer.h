#ifndef __emcClusterContainer_h__
#define __emcClusterContainer_h__

class emcClusterContent;

#include <iosfwd>
#include "phool.h"
#include "PHObject.h"
 
/** (ABC) Container of EMCAL clusters.    

@ingroup interface

 */

class TClonesArray;

class emcClusterContainer : public PHObject
{
public:

  virtual ~emcClusterContainer(){}

  /// Add a new cluster (using default emcClusterContentv# ctor).
  virtual emcClusterContent* addCluster(unsigned int)
  { PHOOL_VIRTUAL_WARNING; return 0; }

  /** Add a new cluster (using copy ctor).
      c must be of compatible type, otherwise this will return 0.
  */
  virtual emcClusterContent* addCluster(unsigned int,
					const emcClusterContent&)
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual emcClusterContent* addCluster(const emcClusterContent &c);

  // the size you'll never exceed for this object.
  virtual unsigned int capacity(void) const 
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual emcClusterContainer* clone(void) const 
  { PHOOL_VIRTUAL_WARNING; return 0; }
 
  virtual emcClusterContainer* create(void) const 
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual emcClusterContent* findCluster(int) const
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual emcClusterContent* getCluster(unsigned int) const
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual void identify(std::ostream&) const
  { PHOOL_VIRTUAL_WARNING; }

  virtual int isValid() const
  { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual void print(std::ostream& os=std::cout) const;

  virtual bool removeCluster(unsigned int)
  { PHOOL_VIRTUAL_WARNING; return false; }

  virtual bool resize(unsigned int)
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
  emcClusterContainer& operator=(const emcClusterContainer&);

 private:

  ClassDef(emcClusterContainer,0) // Array of emcClusterContainer (ABC)
    
};

#endif
