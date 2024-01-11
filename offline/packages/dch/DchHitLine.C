//
//  TKH--Added the time, width, and 
//       distance to the HitLine class.
//                11-25-2001
//
#include "DchHitLine.hh"

DchHitLine::DchHitLine()
{
  PHPoint null(0,0,0);
  PHVector vnull(0,0,0);
 
  basepoint = null;
  direction = vnull;
  id = -1;
  idMirror = -1;
  arm = -1;
  plane = -1;
  side = -1;
  cell = -1;
  time = -1;
  width = -1;
  distance = -1;
  used = 0;
  associatedCandidate = -1;

}

DchHitLine::DchHitLine(int iid, int idmi, short iarm, short iside, short iplane, short icell, 
		       float itime, float iwidth, float idistance,
                       PHPoint point, PHVector vector, PHCylPoint cyl)
{
  basepoint    = point;
  direction    = vector;
  baseLocalCyl = cyl;

  id       = iid;
  idMirror = idmi;
  arm      = iarm;           // copy to DchHitLineyy members
  plane    = iplane;
  cell     = icell;
  side     = iside;
  time     = itime;
  width    = iwidth;
  distance = idistance;
  used     = 0;

  associatedCandidate = -1;
  
}

















