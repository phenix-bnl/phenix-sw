#ifndef __DCHPC1HIT_H
#define __DCHPC1HIT_H

#include "PHPoint.h"
#include "PHCylPoint.h"

class DchPc1Hit : public PHPoint
{
public:
  DchPc1Hit(int id, short arm, short side, short sector, PHPoint globalP);
  virtual ~DchPc1Hit(){};

  int   getSector() { return sector;}
  int   getId() {return id;}
  short getArm() {return arm;}
  short getSide() {return side;}
  int   getAssociatedCandidate() {return associatedCandidate;}
  
  void setId(int val) {id = val;}
  void setArm(short val) {arm = val;}
  void setSide(short val) {side = val;}
  void setAssociatedCandidate(short id) {associatedCandidate = id;}
  
  // for beta and theta calucaulation (for UV Wire)
  void       setLocalIntersectionPoint(PHPoint local) { localInterPoint = local;}
  PHPoint    getLocalIntersectionPoint() {return localInterPoint;}
  
  PHBoolean used;

private:

  int id;                     // some info					
  short arm;					
  short side;					
  short sector;  
  short associatedCandidate;
  
  PHPoint    localInterPoint; // UV local intersection point with the plane defined by X wires

};

#endif /*__DCHPC1HIT_H*/
