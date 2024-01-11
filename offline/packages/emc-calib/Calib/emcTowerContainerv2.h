#ifndef __EMCTOWERCONTAINERV2_H__
#define __EMCTOWERCONTAINERV2_H__

#ifndef __EMCTOWERCONTAINER_H__
#include "emcTowerContainer.h"
#endif
#ifndef __EMCTOWERCONTENTV2_H__
#include "emcTowerContentv2.h" 
#endif
// this latter include is needed so the compiler knows that it derives from
// emcTowerContent and so the addTower and getTower methods
// are considered as overriding methods and not overloading or hiding ones.
#include <map>

class TClonesArray;

/** (VERSION) Container of emcTowerContentv2.
@ingroup calibration
*/

class emcTowerContainerv2 : public emcTowerContainer
{
public:

  emcTowerContainerv2();

  emcTowerContainerv2(const emcTowerContainerv2&);

  emcTowerContainerv2& operator=(const emcTowerContainerv2&);

  emcTowerContainerv2* clone(void) const;

  emcTowerContainerv2* create(void) const;

  virtual ~emcTowerContainerv2();

  unsigned int capacity(void) const;

  emcTowerContentv2* addTower(unsigned int i);

  emcTowerContentv2* addTower(unsigned int i, const emcTowerContent&);

  emcTowerContentv2* findTower(int towerID) const;

  emcTowerContentv2* getTower(unsigned int i) const;

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
  void copy(emcTowerContainerv2& dest) const;

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  mutable std::map<int,int> fTowerIdToIndex; //!
  mutable bool fTowerIdToIndexIsUpToDate; //!

  ClassDef(emcTowerContainerv2,1) // Array of emcTowerContentv2
};
#endif
