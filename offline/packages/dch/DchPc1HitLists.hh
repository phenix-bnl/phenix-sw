//--------------------------------------------------- 
// PHENIX Drift Chamber Software: Stony Brook August 2000    
// 
// Implementation of the Class: 
//
// Description: hold the pointers to the pc1 hit in some convinient manner.
//
//--------------------------------------------------- 
#ifndef __DCHPC1HITLISTS_H
#define __DCHPC1HITLISTS_H

#include "PHPointerList.h"
#include "DchPc1Hit.hh"

class DchPc1HitLists { 

public:
  
  DchPc1HitLists(size_t = 1000);
  virtual ~DchPc1HitLists();

  PHBoolean clearAndDestroy();

  PHPointerList<DchPc1Hit>* getList() {return all;}
  
  size_t lengthOfList(short arm, short side);
  size_t lengthOfList() { return  all->length();}
  PHBoolean append(short arm, short side, DchPc1Hit*);
  PHBoolean append(DchPc1Hit*);

private:

  PHPointerList<DchPc1Hit>* all; // complete list

  PHPointerList<DchPc1Hit>* pc1[2][2]; // partial list divided in arm and side

}; 

#endif /* __DCHPC1HITLISTS_H */ 
