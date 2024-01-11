/////////////////////////////////////
// Author:
// $Log: DchHitLineOutv1.hh,v $
// Revision 1.6  2002/04/03 14:35:53  dave
// Clean ups.  Extra blank lines, commented-out code, removed.
//
// Revision 1.5  2002/02/23 03:59:28  momchil
// merged the latest changes of prod_2_4 into the HEAD branch
//
// Revision 1.2.6.2  2002/02/20 22:13:05  pinkenbu
// Here comes the head branch...
//
// Revision 1.4  2002/02/13 18:39:28  momchil
// Merged the changes from the prod_2 branch
//
// Revision 1.2.6.1  2002/02/05 06:14:02  jjia
// merge changes from main branch
//
// Revision 1.3  2002/02/04 09:01:43  jjia
// Implemented the new output hit table, cosmetic fix to Track and Raw Table
//
//
////////////////////////////////////
#ifndef DCHHITLINEOUTV1_H
#define DCHHITLINEOUTV1_H

#include "PHObject.h"
#include "PHPoint.h"
#include "PHVector.h"
#include "DchHitLineOut.hh"

class DchHitLineOutv1 : public DchHitLineOut
{
protected:
  void Init();
public:

  DchHitLineOutv1() ;
  DchHitLineOutv1(DchHitLineOut*hit);
  virtual ~DchHitLineOutv1(){};
  void print();
  void setUsed(short val) { used = val;}
  void setId(int val) { id = val;}
  void setIdmirror(int val) { idmirror = val;}
  void setArm(short val) { arm = val;}
  void setPlane(short val) {plane = val;}
  void setCell(short val) { cell = val;}
  void setSide(short val) {side = val;}
  void setAssociatedCandidate(int val) { associatedCandidate = val;}

  void setDistance(float val) { distance = val;}
  void setWidth(float val) { width = val;}
  void setIdraw1(short val) {idraw1 = val;}
  void setIdraw2(short val) {idraw2 = val;}
  void setTime1(int val) { time1 = val;}
  void setTime2(int val) { time2 = val;}
  void setXYZ(PHPoint val) { xyz_x = val.getX();xyz_y = val.getY();xyz_z = val.getZ();}
  void setEXYZ(PHPoint val) {exyz_x = val.getX();exyz_y = val.getY();exyz_z = val.getZ();}
  void setVXYZ(PHPoint val) {vxyz_x = val.getX();vxyz_y = val.getY();vxyz_z = val.getZ();}

  short getUsed() { return used;}
  int   getId() { return id;}
  int   getIdmirror() { return idmirror;}
  short getArm() { return arm;}
  short getPlane() { return plane;}
  short getCell() { return cell;}
  short getSide() { return side;}
  int   getAssociatedCandidate() { return associatedCandidate;}
  float getDistance() {return distance;}
  float getWidth() { return width;}
  short getIdraw1() { return idraw1;}
  short getIdraw2() { return idraw2;}
  int   getTime1() { return time1;}
  int   getTime2() { return time2;}
  float getX(){ return xyz_x;}
  float getY(){ return xyz_y;}
  float getZ(){ return xyz_z;}
  PHPoint getXYZ() { PHPoint val(xyz_x,xyz_y,xyz_z); return val;}
  PHPoint getEXYZ() { PHPoint val(exyz_x,exyz_y,exyz_z);return val;}
  PHVector getVXYZ() { PHVector val(vxyz_x,vxyz_y,vxyz_z);return val;}

protected:

  int id;
  int idmirror;
  short arm;					
  short plane;					
  short cell;					
  short side;
  short used;
  int associatedCandidate;

  float distance;
  float width;
  short idraw1;
  short idraw2;
  int time1;
  int time2;
  float  xyz_x,  xyz_y,  xyz_z;
  float exyz_x, exyz_y, exyz_z;
  float vxyz_x, vxyz_y, vxyz_z;
  ClassDef(DchHitLineOutv1,1)
};

#endif /*DCHHITLINEOUTV1_H*/

