/////////////////////////////////////
// Author:
//
// Revision 1.9  2003/03/28 18:47:16  pinkenbu
// Add isValid function, fix dangerous silent Reset in DchHitLineTable base class
//
// Revision 1.8  2002/04/03 14:35:53  dave
// Clean ups.  Extra blank lines, commented-out code, removed.
//
// Revision 1.7  2002/02/23 03:59:29  momchil
// merged the latest changes of prod_2_4 into the HEAD branch
//
// Revision 1.1.2.4  2002/02/21 05:08:00  federica
// (Jiangyong Jia) check in new DchTrackTable
//
// Revision 1.1.2.3  2002/02/20 22:13:05  pinkenbu
// Here comes the head branch...
//
// Revision 1.5  2002/02/20 21:39:29  pinkenbu
// Changes from jjia, use DchHitLineTable as node name for dst output
//
// Revision 1.1.2.2  2002/02/20 00:05:39  jjia
// change the DchHitLine was changed to conform to the new style
//
// Revision 1.1.2.1  2002/02/05 17:28:32  jjia
// merge new file from mail branch
//
// Revision 1.1  2002/02/04 09:01:44  jjia
// Implemented the new output hit table, cosmetic fix to Track and Raw Table
//
//
////////////////////////////////////
#ifndef DCHHITLINETABLEV2_HH
#define DCHHITLINETABLEV2_HH

#include "DchHitLineTable.hh"

#include <iostream>

class DchHitLineOut;
class TClonesArray;

class DchHitLineTablev2: public DchHitLineTable
{
 public:
  DchHitLineTablev2();
  DchHitLineTablev2(int);
  virtual ~DchHitLineTablev2();

  int   Expand(int);
  void  Clear(Option_t *option="");
  void  Reset();
  int isValid() const;

  int   Entries() { return nDCHits;}
  int   Size() { return DchHits->GetSize();}
  void  identify(std::ostream& os = std::cout) const;
  int   AddHit(DchHitLineOut* hit);

  //----------------------------------
  DchHitLineOut* getHit(int i);
  TClonesArray*  getHits(){return DchHits;}
 //_________________________________
  int   getId(int i);
  int   getIdmirror(int i);        
  short getArm(int i);
  short getPlane(int i);
  short getCell(int i);
  short getSide(int i);
  float getDistance(int i);
  float getWidth(int i);
  int   getTime1(int i);
  PHPoint getXYZ(int i);
  void  setId(int i, int val);
  void  setIdmirror(int i, int val);       
  void  setArm(int i, short val);
  void  setPlane(int i, short val);
  void  setCell(int i, short val);
  void  setSide(int i, short val);
  void  setDistance(int i, float val);
  void  setWidth(int i, float val);
  void  setTime1(int i, int val);
  void  setXYZ(int i, PHPoint val);

  //------ not used for short table
  short getUsed(int i);
  short getAssociatedCandidate(int i);
  short getIdraw1(int i);
  short getIdraw2(int i);
  int   getTime2(int i);
  PHPoint getEXYZ(int i);
  PHVector getVXYZ(int i);
  void  setUsed(int i, short val);
  void  setAssociatedCandidate(int i, short val);
  void  setIdraw1(int i, short val);
  void  setIdraw2(int i, short val);
  void  setTime2(int i, int val);
  void  setEXYZ(int i, PHPoint val);
  void  setVXYZ(int i, PHVector val);
  //--------------------------------
  void   setStatus(int val){hitstat =val;}
  int    getStatus()       {return hitstat;}

 protected: 
  int nDCHits;
  int hitstat;
  TClonesArray *DchHits;
  ClassDef(DchHitLineTablev2,1)

};

#endif



