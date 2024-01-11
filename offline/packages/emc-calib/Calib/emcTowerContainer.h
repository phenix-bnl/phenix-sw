#ifndef __EMCTOWERCONTAINER_H__
#define __EMCTOWERCONTAINER_H__

class emcTowerContent;

#ifndef __PHOBJECT_H__
#include "PHObject.h"
#endif

/** (ABC) Container of emcTowerContent objects.

This is the object you'll find in DSTs, as far as EMCAL towers are concerned.

Basic usage is as follows :

\code
void loop(const emcTowerContainer& tc)
{
  for ( size_t i = 0; i < lc.size(); ++lc ) 
  {
    emcTowerContent* onetower = lc->getTower(i);
    // use then onetower's emcTowerContent interface...
  }
}
\endcode

\note
Do not mix getTower(index) and findTower(towerid) methods. 
- In the first one the index ranges from 0 to size(). 
If the index is correct, you are guaranteed to get a non null pointer as a return value.
- In the second one, towerid ranges from 0 to 24767, and the return value may well be 0 if that specific tower is not in the container.

    @ingroup interface
    @ingroup calibration
    @ingroup clustering
 */

class emcTowerContainer : public PHObject
{
public:

  virtual ~emcTowerContainer();

  /// Add a new (using default ctor) tower.
  virtual emcTowerContent* addTower(unsigned int /*index*/) 
  { warning("addTower(i)"); return 0; }

  /** Add a new (using copy ctor) tower. 
      t must be of a compatible type, otherwise this will return 0.*/  
  virtual emcTowerContent* addTower(unsigned int /*index*/, 
				    const emcTowerContent& /*t*/)
  { warning("addTower(i,towercontent)"); return 0; }

  /// the size you'll never exceed for this object.
  virtual unsigned int capacity(void) const 
  { warning("capacity"); return 0; }

  /// Make a copy of this object.
  virtual emcTowerContainer* clone(void) const 
  { warning("clone"); return 0; }

  /// Make an empty copy of this object (i.e. copy only the type).
  virtual emcTowerContainer* create(void) const
  { warning("create"); return 0; }

  /// Return a given tower (or 0 if not found) by its towerid.
  virtual emcTowerContent* findTower(int /*towerID*/) const
  { warning("findTower"); return 0; }

  /// Return a given tower (or 0 if not found) by its index.
  virtual emcTowerContent* getTower(unsigned int /*i*/) const 
  { warning("getTower"); return 0; }

  virtual void identify(std::ostream& os=std::cout) const;

  virtual int isValid() const;

  virtual void print(std::ostream& os=std::cout, int level=0) const;

  virtual bool removeTower(unsigned int /*i*/) 
  { warning("removeTower"); return false; }

  /// Set a new size for this container. Old data are lost.
  virtual bool resize(unsigned int /*newsize*/) 
  { warning("resize"); return false; }

  // Empty the container.
  virtual void Reset();

  /// the actual size of the object.
  virtual unsigned int size(void) const 
  { warning("size"); return 0; }

protected:

  /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
  emcTowerContainer& operator=(const emcTowerContainer&);

private:

  void warning(const char* method) const 
  { 
    std::cerr << __FILE__ << ":" << __LINE__ << method
	      << " NOT IMPLEMENTED in daughter"
	      << " class !" << std::endl; 
  }

  ClassDef(emcTowerContainer,0) // Array of emcTowerContainer (ABC)
    
};

#endif
