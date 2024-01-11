//
//  TKH--Update to add time width
//       and distance to the HitLine.
//           11-25-2001
//
#ifndef __DCHHITLINE_H
#define __DCHHITLINE_H

#include "PHLine.h"
#include "PHCylPoint.h"

class DchHitLine : public PHLine
{
public:

  DchHitLine();

  DchHitLine(int id, int idMirror, short arm, 
	     short side, short plane, short cell,
             float time, float width, float distance,
	     PHPoint globalP, PHVector globalV, PHCylPoint localP);
  virtual ~DchHitLine(){};

  int   getId()       {return id;}
  short getArm()      {return arm;}
  short getSide()     {return side;}
  short getPlane()    {return plane;}
  short getCell()     {return cell;}
  int   getIdmirror() {return idMirror;}
  float getTime()     {return time;}
  float getWidth()    {return width;}
  float getDistance() {return distance;}
  int   getAssociatedCandidate() {return associatedCandidate;}
  
  void setId(int val)       {id = val;}
  void setArm(short val)    {arm = val;}
  void setSide(short val)   {side = val;}
  void setCell(short val)   {cell = val;}
  void setPlane(short val)  {plane = val;}
  void setIdmirror(int val) {idMirror = val;}
  void setTime(int val)     {time = val;}
  void setWidth(int val)    {width = val;}
  void setDistance(int val) {distance = val;}
  void setAssociatedCandidate(short id) {associatedCandidate = id;}
  
  // for alpha and phi calculation (for X Wires)
  void setLocalPoint(PHCylPoint pnt) {baseLocalCyl = pnt;}
  PHCylPoint getLocalPoint()    { return baseLocalCyl;}
  PHAngle    getLocalPhi()      { return baseLocalCyl.getPhi();}
  double     getLocalRadius()   { return baseLocalCyl.getR();}

  // for beta and theta calucaulation (for UV Wire)
  void       setLocalIntersectionPoint(PHPoint local) { localInterPoint = local;}
  PHPoint    getLocalIntersectionPoint() {return localInterPoint;}

  void       setGlobalIntersectionPoint(PHPoint global) { globalInterPoint = global;}
  PHPoint    getGlobalIntersectionPoint() {return globalInterPoint;}
  
  PHBoolean used;

private:

  int id,idMirror; // some info					
  short arm;					
  short plane;					
  short cell;					
  short side;
  float time;
  float width;
  float distance;
  
  short associatedCandidate;
  
  PHCylPoint baseLocalCyl;    // X hit local cylindrical coordinates
  PHPoint localInterPoint;    // UV local intersection point with the plane defined by X wires
  PHPoint globalInterPoint;   // UV global intersection point with the plane defined by X wires

};

#endif /* __DCHHITLINE_H*/
