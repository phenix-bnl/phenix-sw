//--------------------------------------------------- 
// PHENIX Drift Chamber Software: Stony Brook  April 29  15:32:11 2000    
// 
// Implementation of the Class: DchHitLineLists
//
// Description: hold the pointers to the hit line in some convinient manner.
// the Idea is to limit the time going in unnecessary loops !!!
//
// Created by: Federica Ceretto/Messer  at April 29  15:32:11 2000 
//
//--------------------------------------------------- 
#ifndef __DCHHITLINELISTS_H
#define __DCHHITLINELISTS_H

#include "PHPointerList.h"
#include "DchHitLine.hh"
#include "DchAnaPar.h"

class DchHitLineLists { 

public:
  
  //! constructor
  DchHitLineLists(size_t = 1000);
  
  //! destructor
  virtual ~DchHitLineLists();

  PHBoolean clearAndDestroy();
  PHBoolean append(short wireType, short arm, short side, DchHitLine*);
  PHBoolean append(DchHitLine*);
  PHBoolean merge();

  PHPointerList<DchHitLine>* getList(short arm, short side, short plane, short cell);
  PHPointerList<DchHitLine>* getList(short type, short arm ,short side);
  PHPointerList<DchHitLine>* getList() {return all;}
  
  size_t lengthOfList(short arm, short side,short plane,short cell);
  size_t lengthOfList(short wireType, short arm, short side);
  size_t lengthOfList(short wireType, short arm);
  size_t lengthOfList() { return  all->length();}

  void sortInCellPartialLists();
  void sortInPlanePartialLists();

private:

  PHPointerList<DchHitLine>* all; // complete list

  PHPointerList<DchHitLine>* x1[2][2]; // partial list divided in arm and side
  PHPointerList<DchHitLine>* x2[2][2];
  PHPointerList<DchHitLine>* uv1[2][2];
  PHPointerList<DchHitLine>* uv2[2][2];
  
  PHPointerList<DchHitLine>* x1all[2];   // partial list divided in arm only
  PHPointerList<DchHitLine>* x2all[2];   
  PHPointerList<DchHitLine>* uv1all[2];   
  PHPointerList<DchHitLine>* uv2all[2];   

  PHPointerList<DchHitLine>* list[2][2][40][80]; // very partial list for sorting purpuses
}; 

#endif /* __DCHHITLINELISTS_H */ 
