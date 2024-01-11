#ifndef __EMCTOWERCONTAINERV3_H__
#define __EMCTOWERCONTAINERV3_H__

#ifndef __EMCTOWERCONTAINER_H__
#include "emcTowerContainer.h"
#endif
#ifndef __EMCTOWERCONTENTV3_H__
#include "emcTowerContentv3.h" 
#endif
// this latter include is needed so the compiler knows that it derives from
// emcTowerContent and so the addTower and getTower methods
// are considered as overriding methods and not overloading or hiding ones.
#include <map>

class TClonesArray;

/** (VERSION) Container of emcTowerContentv3.
@ingroup calibration
*/

class emcTowerContainerv3 : public emcTowerContainer
{
public:

  emcTowerContainerv3();

  emcTowerContainerv3(const emcTowerContainerv3&);

  emcTowerContainerv3& operator=(const emcTowerContainerv3&);

  emcTowerContainerv3* clone(void) const;

  emcTowerContainerv3* create(void) const;

  virtual ~emcTowerContainerv3();

  unsigned int capacity(void) const;

  emcTowerContentv3* addTower(unsigned int i);

  emcTowerContentv3* addTower(unsigned int i, const emcTowerContent&);

  emcTowerContentv3* findTower(int towerID) const;

  emcTowerContentv3* getTower(unsigned int i) const;

  void identify(std::ostream& os=std::cout) const;

  int isValid() const;

  bool removeTower(unsigned int i);

  void Reset();

  bool resize(unsigned int newsize);

  unsigned int size(void) const;

protected:

  TClonesArray* fEmcTowers;

private:
  void allocate(unsigned int thesize);
  void copy(emcTowerContainerv3& dest) const;
  bool expand(unsigned int);
  bool expand_for(unsigned int);

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  mutable std::map<int,int> fTowerIdToIndex; //!
  mutable bool fTowerIdToIndexIsUpToDate; //!

  ClassDef(emcTowerContainerv3,1) // Array of emcTowerContentv3
};
#endif
