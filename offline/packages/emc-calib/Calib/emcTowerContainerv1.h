#ifndef __EMCTOWERCONTAINERV1_H__
#define __EMCTOWERCONTAINERV1_H__

#ifndef __EMCTOWERCONTAINER_H__
#include "emcTowerContainer.h"
#endif
#ifndef __EMCTOWERCONTENTV1_H__
#include "emcTowerContentv1.h" 
#endif
// this latter include is needed so the compiler knows that it derives from
// emcTowerContent and so the addTower and getTower methods
// are considered as overriding methods and not overloading or hiding ones.

class TClonesArray;

/** (VERSION) Container of emcTowerContentv1. 
@ingroup calibration
*/

class emcTowerContainerv1 : public emcTowerContainer
{
public:

  emcTowerContainerv1();

  emcTowerContainerv1(const emcTowerContainerv1&);

  emcTowerContainerv1& operator=(const emcTowerContainerv1&);

  emcTowerContainerv1* clone(void) const;

  emcTowerContainerv1* create(void) const;
  
  virtual ~emcTowerContainerv1();

  unsigned int capacity(void) const;

  emcTowerContentv1* addTower(unsigned int i);

  emcTowerContentv1* addTower(unsigned int i, const emcTowerContent&);

  emcTowerContentv1* findTower(int towerID) const;

  emcTowerContentv1* getTower(unsigned int i) const;

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
  void copy(emcTowerContainerv1& dest) const;

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  ClassDef(emcTowerContainerv1,1) // Array of emcTowerContentv1
};
#endif
