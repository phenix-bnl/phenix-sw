#ifndef __DCHHITLINETABLEV1_H
#define __DCHHITLINETABLEV1_H

#include <iostream>
#include "TClonesArray.h"
#include "DchHitLineTable.hh"
#include "DchHitLineOut.hh"
#include "DchHitLineOutv1.hh"

class DchHitLineTablev1: public DchHitLineTable
{
 public:
  DchHitLineTablev1();
  DchHitLineTablev1(int);
  virtual ~DchHitLineTablev1();

  int   Expand(int);
  void  Clear(Option_t *option="");
  void  Reset();
  int isValid() const;

  void  identify(std::ostream& os = std::cout) const;
  int   Entries() { return nDCHits;}
  int   Size() { return DchHits->GetSize();}
  int   AddHit(DchHitLineOut* hit);

  DchHitLineOut* getHit(int i);
  TClonesArray*  getHits(){return DchHits;}
  //-----------------------------
  short getUsed(int i);
  int getId(int i);
  int getIdmirror(int i);          
  short getArm(int i);
  short getPlane(int i);
  short getCell(int i);
  short getSide(int i);
  
  short getAssociatedCandidate(int i);
  
  float getDistance(int i);
  float getWidth(int i);
  short getIdraw1(int i);
  short getIdraw2(int i);
  int getTime1(int i);
  int getTime2(int i);
  PHPoint getXYZ(int i);
  PHPoint getEXYZ(int i);
  PHVector getVXYZ(int i);
 
  //------

  void  setUsed(int i, short val);
  void  setId(int i, int val);
  void  setIdmirror(int i, int val);       
  void  setArm(int i, short val);
  void  setPlane(int i, short val);
  void  setCell(int i, short val);
  void  setSide(int i, short val);
  
  void  setAssociatedCandidate(int i, short val);
  
  void  setDistance(int i, float val);
  void  setWidth(int i, float val);
  void  setIdraw1(int i, short val);
  void  setIdraw2(int i, short val);
  void  setTime1(int i, int val);
  void  setTime2(int i, int val);
  void  setXYZ(int i, PHPoint val);
  void  setEXYZ(int i, PHPoint val);
  void  setVXYZ(int i, PHVector val);
  //--------------------------------
  void   setStatus(int val){hitstat =val;}
  int    getStatus()       {return hitstat;}
protected:  
  int nDCHits;
  int hitstat;
  TClonesArray *DchHits;

  ClassDef(DchHitLineTablev1,1)

};
#endif

