#include "DchPc1Hit.hh"

DchPc1Hit::DchPc1Hit(int iid,short iarm, short iside, short isector, PHPoint point)
{
  setX(point.getX());
  setY(point.getY());
  setZ(point.getZ());
 
  id       = iid;
  arm      = iarm;           
  side     = iside;
  sector   = isector;
  used     = 0;
 
  associatedCandidate = -1;
  
}






