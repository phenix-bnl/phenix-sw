#include "DchPc1HitLists.hh"

DchPc1HitLists::DchPc1HitLists(size_t totalHits)
{
  all = new PHPointerList<DchPc1Hit>(totalHits);
  for (int a=0; a<2; a++) {
    for (int s=0; s<2; s++) {
      pc1[a][s]  = new PHPointerList<DchPc1Hit>(totalHits/20);
    }
  }
}

DchPc1HitLists::~DchPc1HitLists()
{
  all->clearAndDestroy();
  for (int a=0; a<2; a++) {
    for (int s=0; s<2; s++) {
      delete pc1[a][s];
    }
  }
  delete all;
}

PHBoolean 
DchPc1HitLists::clearAndDestroy()
{
  all->clearAndDestroy();
  for (int a=0; a<2; a++) {
    for (int s=0; s<2; s++) {
      pc1[a][s]->clear();
    }
  } 
  return True;
}
 
PHBoolean 
DchPc1HitLists::append(DchPc1Hit* hit)
{  
  int arm    = hit->getArm();
  int side   = hit->getSide();

  all->append(hit);
  pc1[arm][side]->append(hit);

  return True;
}

PHBoolean 
DchPc1HitLists::append(short arm, short side, DchPc1Hit* hit)
{
  pc1[arm][side]->append(hit);
  return True;
}

size_t 
DchPc1HitLists::lengthOfList(short arm, short side)
{
  return pc1[arm][side]->length();
}


