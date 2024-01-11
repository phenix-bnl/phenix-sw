// Created by: Jeffery T. Mitchell
// Description: Abstract class for PHENIX track models.

#include <iostream>
#include <PHGeometry.h>
#include <PHTrack.h>

using namespace std;
using namespace PHGeometry;

PHTrack::PHTrack()
{
  trackIndex = -1;
}

PHTrack::~PHTrack()
{
  polyLine.clearAndDestroy();
  projections.clearAndDestroy();
  projErrors.clearAndDestroy();
  directions.clearAndDestroy();
  ifIntersect.clearAndDestroy();
}

// Predict the momentum
PHVector 
PHTrack::predictMomentum()
{
  PHVector mom;

  mom.setX(0.0);
  mom.setY(0.0);
  mom.setZ(0.0);

  return mom;
}

// Calculate the path length to the CRK
double 
PHTrack::pathLengthToCrk()
{
  double path;

  path = 0.0;
  cerr << "PHTrack::pathLengthToCrk: Not defined.  Returning 0.\n";
  return path;

}

// Calculate the polyline for the track model
void 
PHTrack::calcPolyLine()
{
  polyLine.clearAndDestroy();

  cerr << "PHTrack::calcPolyLine:  Not defined.  Returning 0.\n";
}

void 
PHTrack::callProjections()
{
  // THE ORDER OF THE PROJECTIONS, DIRECTIONS, AND BOOLEAN FLAGS ARE AS
  // IN THE HEADER FILE:

  //  0: virtual PHBoolean projectToVertex(PHLine&, PHPoint&);
  //  1: virtual PHBoolean projectToDch(PHLine&, PHPoint&);
  //  2: virtual PHBoolean projectToPc1(PHLine&, PHPoint&);
  //  3: virtual PHBoolean projectToPc2(PHLine&, PHPoint&);
  //  4: virtual PHBoolean projectToPc3(PHLine&, PHPoint&);
  //  5: virtual PHBoolean projectToCrk(PHLine&, PHPoint&);
  //  6: virtual PHBoolean projectToTec(PHLine&, PHPoint&);
  //  7: virtual PHBoolean projectToTof(PHLine&, PHPoint&);
  //  8: virtual PHBoolean projectToPbSc(PHLine&, PHPoint&);
  //  9: virtual PHBoolean projectToPbGl(PHLine&, PHPoint&);
  // 10: removed tzr
  // 11: removed pcr
  // 12: virtual PHBoolean projectToAcc(PHLine&, PHPoint&);
  // 13: virtual PHBoolean projectToTofw(PHLine&, PHPoint&);
  // 14~17: virtual PHBoolean projectToSvx(const short ilayer, PHLine&, PHPoint&);
  // 18: virtual PHBoolean projectToHbd(PHLine&, PHPoint&);

  projections.clearAndDestroy();
  projErrors.clearAndDestroy();
  directions.clearAndDestroy();
  ifIntersect.clearAndDestroy();

  PHLine proj;
  PHPoint err;

  PHPoint *p;
  PHVector *v;
  PHPoint *e;
  PHBoolean *ifProject;

  //0
  if (projectToVertex(proj,err)) 
    {
      p = new PHPoint(proj.getBasepoint());
      v = new PHVector(proj.getDirection());
      e = new PHPoint(err);
      v->normalize();
      directions.append(v);
      projections.appendPoint(p);
      projErrors.appendPoint(e);
      ifProject = new PHBoolean(True);
    } 
  else 
    {
      ifProject = new PHBoolean(False);
    }
  ifIntersect.append(ifProject);

  //1
  if (projectToDch(proj,err)) 
    {
      p = new PHPoint(proj.getBasepoint());
      v = new PHVector(proj.getDirection());
      e = new PHPoint(err);
      v->normalize();
      directions.append(v);
      projections.appendPoint(p);
      projErrors.appendPoint(e);
      ifProject = new PHBoolean(True);
  } else 
    {
      ifProject = new PHBoolean(False);
    }
  ifIntersect.append(ifProject);

  //2
  if (projectToPc1(proj,err)) 
    {
      p = new PHPoint(proj.getBasepoint());
      v = new PHVector(proj.getDirection());
      e = new PHPoint(err);
      v->normalize();
    directions.append(v);
    projections.appendPoint(p);
    projErrors.appendPoint(e);
    ifProject = new PHBoolean(True);
    }
  else 
    {
      ifProject = new PHBoolean(False);
    }
  ifIntersect.append(ifProject);
  
  //3
  if (projectToPc2(proj,err)) 
    {
      p = new PHPoint(proj.getBasepoint());
      v = new PHVector(proj.getDirection());
      e = new PHPoint(err);
      v->normalize();
      directions.append(v);
      projections.appendPoint(p);
      projErrors.appendPoint(e);
      ifProject = new PHBoolean(True);
    } 
  else 
    {
      ifProject = new PHBoolean(False);
    }
  ifIntersect.append(ifProject);
  
  //4
  if (projectToPc3(proj,err)) 
    {
      p = new PHPoint(proj.getBasepoint());
      v = new PHVector(proj.getDirection());
      e = new PHPoint(err);
      v->normalize();
      directions.append(v);
      projections.appendPoint(p);
      projErrors.appendPoint(e);
      ifProject = new PHBoolean(True);
    }
  else 
    {
      ifProject = new PHBoolean(False);
    }
  ifIntersect.append(ifProject);
  
  //5
  if (projectToCrk(proj,err)) 
    {
      p = new PHPoint(proj.getBasepoint());
      v = new PHVector(proj.getDirection());
      e = new PHPoint(err);
      v->normalize();
      directions.append(v);
      projections.appendPoint(p);
      projErrors.appendPoint(e);
      ifProject = new PHBoolean(True);
    } 
  else 
    {
      ifProject = new PHBoolean(False);
    }
  ifIntersect.append(ifProject);

  //6
  if (projectToTec(proj,err)) 
    {
      p = new PHPoint(proj.getBasepoint());
      v = new PHVector(proj.getDirection());
      e = new PHPoint(err);
      v->normalize();
      directions.append(v);
      projections.appendPoint(p);
      projErrors.appendPoint(e);
      ifProject = new PHBoolean(True);
    } 
  else 
    {
      ifProject = new PHBoolean(False);
    }
  ifIntersect.append(ifProject);
  
  //7
  if (projectToTof(proj,err)) 
    {
      p = new PHPoint(proj.getBasepoint());
      v = new PHVector(proj.getDirection());
      e = new PHPoint(err);
      v->normalize();
      directions.append(v);
      projections.appendPoint(p);
      projErrors.appendPoint(e);
      ifProject = new PHBoolean(True);
    } 
  else 
    {
      ifProject = new PHBoolean(False);
    }
  ifIntersect.append(ifProject);
  
  //8
  if (projectToPbSc(proj,err)) 
    {
      p = new PHPoint(proj.getBasepoint());
      v = new PHVector(proj.getDirection());
      e = new PHPoint(err);
      v->normalize();
      directions.append(v);
      projections.appendPoint(p);
      projErrors.appendPoint(e);
      ifProject = new PHBoolean(True);
    } 
  else 
    {
      ifProject = new PHBoolean(False);
    }
  ifIntersect.append(ifProject);

  //9
  if (projectToPbGl(proj,err)) 
    {
      p = new PHPoint(proj.getBasepoint());
      v = new PHVector(proj.getDirection());
      e = new PHPoint(err);
      v->normalize();
      directions.append(v);
      projections.appendPoint(p);
      projErrors.appendPoint(e);
      ifProject = new PHBoolean(True);
    } 
  else 
    {
      ifProject = new PHBoolean(False);
    }
  ifIntersect.append(ifProject);

  //10
      ifProject = new PHBoolean(False);
  ifIntersect.append(ifProject);

  //11
      ifProject = new PHBoolean(False);
  ifIntersect.append(ifProject);

  //12
  // aerogel projections are not used
      ifProject = new PHBoolean(False);
  ifIntersect.append(ifProject);

  //13 tofw
  if (projectToTofw(proj,err)) 
    {
      p = new PHPoint(proj.getBasepoint());
      v = new PHVector(proj.getDirection());
      e = new PHPoint(err);
      v->normalize();
      directions.append(v);
      projections.appendPoint(p);
      projErrors.appendPoint(e);
      ifProject = new PHBoolean(True);
    } 
  else 
    {
      ifProject = new PHBoolean(False);
    }
  ifIntersect.append(ifProject);

// SVX
  for(int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++)
    {
      if (projectToSvx(ilayer, proj,err))
        {
          p = new PHPoint(proj.getBasepoint());
          v = new PHVector(proj.getDirection());
          e = new PHPoint(err);
          v->normalize();
          directions.append(v);
          projections.appendPoint(p);
          projErrors.appendPoint(e);
          ifProject = new PHBoolean(True);
        }
      else
        {
          ifProject = new PHBoolean(False);
        }
     ifIntersect.append(ifProject);
    }
  
  if (projectToHbd(proj,err)) 
    {
      p = new PHPoint(proj.getBasepoint());
      v = new PHVector(proj.getDirection());
      e = new PHPoint(err);
      v->normalize();
      directions.append(v);
      projections.appendPoint(p);
      projErrors.appendPoint(e);
      ifProject = new PHBoolean(True);
    } 
  else 
    {
      ifProject = new PHBoolean(False);
    }
  ifIntersect.append(ifProject);

}

PHBoolean 
PHTrack::projectToPlane(const PHPlane &plane, PHLine &proj)
{
  PHPoint point;
  PHVector vector;
  PHBoolean intTest;

  intTest = intersectionPolyLinePlane(polyLine,plane,point);
  vector.setX(0.0);
  vector.setY(0.0);
  vector.setZ(0.0);
  proj.setBasepoint(point);
  proj.setDirection(vector);

  return intTest;
}

// Default method to project the track to a cylinder
short 
PHTrack::projectToCylinder(const PHCylinder &cyl, 
			   PHLine &proj1, PHLine &proj2)
{
  PHPoint point1,point2;
  PHVector vector;
  short intTest;

  intTest = intersectionPolyLineCylinder(polyLine,cyl,point1,point2);
  vector.setX(0.0);
  vector.setY(0.0);
  vector.setZ(0.0);
  proj1.setBasepoint(point1);
  proj1.setDirection(vector);
  proj2.setBasepoint(point2);
  proj2.setDirection(vector);

  return intTest;
} 

// Default method to project the track to a sphere
short 
PHTrack::projectToSphere(const PHSphere &sphere, 
			 PHLine &proj1, PHLine &proj2)
{
  PHPoint point1,point2;
  PHVector vector;
  short intTest;

  intTest = intersectionPolyLineSphere(polyLine,sphere,point1,point2);
  vector.setX(0.0);
  vector.setY(0.0);
  vector.setZ(0.0);
  proj1.setBasepoint(point1);
  proj1.setDirection(vector);
  proj2.setBasepoint(point2);
  proj2.setDirection(vector);

  return intTest;
}













