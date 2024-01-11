#ifndef __EMCTOWERCONTAINERDST_H__
#define __EMCTOWERCONTAINERDST_H__

#ifndef __EMCTOWERCONTAINER_H__
#include "emcTowerContainer.h"
#endif
#ifndef __EMCTOWERCONTENTDST_H__
#include "emcTowerContentDST.h" 
#endif
// this latter include is needed so the compiler knows that it derives from
// emcTowerContent and so the addTower and getTower methods
// are considered as overriding methods and not overloading or hiding ones.
#include <map>

class TClonesArray;

/** (VERSION) Container of emcTowerContentDST.
@ingroup calibration
*/

class emcTowerContainerDST : public emcTowerContainer
{
public:

  emcTowerContainerDST();

  emcTowerContainerDST(const emcTowerContainerDST&);

  emcTowerContainerDST& operator=(const emcTowerContainerDST&);

  emcTowerContainerDST* clone(void) const;

  emcTowerContainerDST* create(void) const;

  virtual ~emcTowerContainerDST();

  unsigned int capacity(void) const;

  emcTowerContentDST* addTower(unsigned int i);

  emcTowerContentDST* addTower(unsigned int i, const emcTowerContent&);

  emcTowerContentDST* findTower(int towerID) const;

  emcTowerContentDST* getTower(unsigned int i) const;

  void identify(std::ostream& os=std::cout) const;

  int isValid() const;

  bool removeTower(unsigned int i);

  void Reset();

  bool resize(unsigned int newsize);

  unsigned int size(void) const;
  float getVertex(const int i) const { if ( i>=0 && i< 3) return vertex[i]; return 0;}; 
  void setVertex( const int i, const float v) {  if ( i>=0 && i< 3) vertex[i] = v;}; 

protected:

  float vertex[3];
  TClonesArray* fEmcTowers;

private:
  void allocate(unsigned int thesize);
  void copy(emcTowerContainerDST& dest) const;
  bool expand(unsigned int);
  bool expand_for(unsigned int);

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  mutable std::map<int,int> fTowerIdToIndex; //!
  mutable bool fTowerIdToIndexIsUpToDate; //!

  ClassDef(emcTowerContainerDST,1) // Array of emcTowerContentDST
};
#endif
