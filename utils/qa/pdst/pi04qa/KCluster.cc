#include "KCluster.h"
#include <math.h>

ClassImp(KCluster)

void KCluster::setArmSecIyIz()
{
  karm = arm();
  sec = sector();
  iy = iypos();
  iz = izpos();
  
  if (karm==1) 
    {
      sec = 7 - sec;
    }
}  

void KCluster::setLocalPos(mEmcGeometryModule *EmcGeo)
{
  lx = -1; ly = -1; lz = -1;
  EmcGeo->GlobalToLocal(x(),y(),z(),sec,lx,ly,lz);
}

void KCluster::setTrackVector()
{
  float trkLength, px, py, pz;

  trkLength = sqrt(x()*x() + y()*y() + (z()-vtxZ)*(z()-vtxZ));
  xyz_unit[0] = x()/trkLength;
  xyz_unit[1] = y()/trkLength;
  xyz_unit[2] = (z()-vtxZ)/trkLength;

  px = ecore() * xyz_unit[0];
  py = ecore() * xyz_unit[1];
  pz = ecore() * xyz_unit[2]; 

  pt = sqrt(px*px + py*py);
}
    
void KCluster::passTOFCut()
{
  TOFCut = 0;
  if (sec < 6)
    {
      if ( fabs(tofcorr()) < 1.35 )
	{
	  TOFCut = 1;
	  if ( fabs(tofcorr()) < 0.9 )
	    {
	      TOFCut = 2;
	    }
	}
    }
  else
    {
      if ( fabs(tofcorr()) < 1.8 )
	{
	  TOFCut = 1;
	}
    }
}

void KCluster::passCHI2Cut()
{
  chi2Cut = 0;
  if ( chi2() < 3.0 ) 
    {
      chi2Cut = 1;
    }      
}      

void KCluster::passTwrhitCut()
{
  twrhitCut = 0;
  if ((sec < 6 && ecore() < 1.0) || (sec > 5 && ecore() < 1.0))
    {
      if ( multiplicity() < 10. ) 
	{
	  twrhitCut = 1;
	}
    }
  else
    {
      if ( multiplicity() < 20. ) 
	{
	  twrhitCut = 1;
	}      
    }
}      

bool KCluster::passFiducialCuts()
{
  if ( lx < 8. || lx > 389. || ly < 2. || ly > 191. ) 
     {
       return false;
     }

  return true;
}      



