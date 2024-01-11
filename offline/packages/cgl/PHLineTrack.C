// Created by: Julia Velkovska
// Purpose: Line track model to be used for B=0 tracks

#include <PHLineTrack.hh>
#include <cglDetectorGeo.hh>
#include <dDchTracksWrapper.h>

using namespace PHGeometry;

// Constructors for PHLineTrack
PHLineTrack::PHLineTrack():
    PHTrack()
{
  long i0 = 0;
  verbosity = 0;
  setTrackIndex (i0);
  set_arm (-1);
}

PHLineTrack::PHLineTrack(const PHPoint & base , const PHVector & dir ):
    PHTrack()
{
  PHLine line;
  line.setBasepoint(base);
  line.setDirection(dir);
  itsLine = line;
  long i0 = 0;
  verbosity = 0; 
  setTrackIndex(i0);
  set_arm( -1);
}

PHLineTrack::PHLineTrack(const PHPoint & p1, const PHPoint & p2):
    PHTrack()
{
  PHLine line(p1, p2);
  itsLine = line;
  long i0 = 0;
  verbosity = 0;  
  set_arm( -1);
  setTrackIndex(i0);
}

PHLineTrack::PHLineTrack(const PHLine & line):
    PHTrack()
{
  itsLine = line;
  long i0 = 0;
  verbosity = 0;  
  setTrackIndex(i0);
  set_arm( -1);
}

PHLineTrack::PHLineTrack(unsigned long &iDch,
                         dDchTracksWrapper* dchTrack,
                         cglDetectorGeo &cglDetGeo)
{
  arm = dchTrack->get_arm(iDch);
  refGeo = &cglDetGeo;
  setLine(iDch, dchTrack);
  setTrackIndex(iDch);
}

//   method to project the track to the vertex
PHBoolean
PHLineTrack::projectToVertex(PHLine &proj, PHPoint &err)
{
  // This method will return instead of the error, the point of
  // closest approach on the line of the track to the z axis.

  PHPoint null(0., 0., 0.);
  err = null;

  PHPoint origin(0, 0, 0);
  PHVector zvector(0, 0, 1);
  PHLine zAxis(origin, zvector);

  PHPoint closest = closestApproachLineLine(zAxis, itsLine);
  proj.setBasepoint(closest);
  proj.setDirection(itsLine.getDirection());

  return True;
}

//   method to project the track to the dch
PHBoolean
PHLineTrack::projectToDch(PHLine &proj, PHPoint &error)
{
  // Have to setLine( int,dDchTracksWrapper * ) before calling this method
  proj = itsLine;
  error.setX(0.0);
  error.setY(0.0);
  error.setZ(0.0);

  return True;
}

PHBoolean
PHLineTrack::projectToPc1(PHLine &intersection, PHPoint &error)
{
  short nPc1Geo;  // number of retrieved PC1 geometry objects
  int igeo = 0;       // geometry loop variable
  PHBoolean itest = 0;
  PHBoolean projectionFound;
  PHPanel pc1Plane[8];


  // Zero out the inputs
  error.setX(0.0);
  error.setY(0.0);
  error.setZ(0.0);

  // Fetch the geometry objects for PC1
  nPc1Geo = refGeo->get_pc1Geo(arm, pc1Plane);

  while (itest == 0 && igeo < nPc1Geo)
    {
      projectionFound = projectToPanel(pc1Plane[igeo], intersection);
      if (projectionFound)
        {
	  itest = 1;
	}
      igeo++;
    }

  return itest;
}

//   method to project the track to the PC2
PHBoolean
PHLineTrack::projectToPc2(PHLine &intersection, PHPoint &error)
{
  short nPc2Geo;  // number of retrieved PC2 geometry objects
  int igeo;       // geometry loop variable
  PHBoolean itest;
  PHBoolean projectionFound;
  PHPanel pc2Plane[8];


  // Zero out the inputs
  error.setX(0.0);
  error.setY(0.0);
  error.setZ(0.0);

  // Fetch the geometry objects for PC2
  nPc2Geo = refGeo->get_pc2Geo(arm, pc2Plane);

  // Loop over all geometry objects until there is an intersection
  itest = 0;
  igeo = 0;
  while (itest == 0 && igeo < nPc2Geo)
    {

      projectionFound = projectToPanel(pc2Plane[igeo], intersection);
      if (projectionFound)
        itest = 1;
      igeo++;

    }

  return itest;
}

PHBoolean
PHLineTrack::projectToPc3(PHLine & intersection, PHPoint & error)
{
  short nPc3Geo;  // number of retrieved PC3 geometry objects
  int igeo;       // geometry loop variable
  PHBoolean itest;
  PHBoolean projectionFound;
  PHPanel pc3Plane[8];

  // Zero out the inputs
  error.setX(0.0);
  error.setY(0.0);
  error.setZ(0.0);

  // Fetch the geometry objects for PC3
  nPc3Geo = refGeo->get_pc3Geo(arm, pc3Plane);

  // Loop over all geometry objects until there is an intersection
  itest = 0;
  igeo = 0;
  while (itest == 0 && igeo < nPc3Geo)
    {

      projectionFound = projectToPanel(pc3Plane[igeo], intersection);
      if (projectionFound)
        itest = 1;
      igeo++;

    } 

  return itest;
}


//   method to project the track to the SVX
PHBoolean
PHLineTrack::projectToSvx(const short ilayer, PHLine & intersection, PHPoint & error)
{
  short nSvxGeo;  // number of retrieved SVX geometry objects
  int igeo;       // geometry loop variable
  PHBoolean itest;
  PHBoolean projectionFound;
  PHPanel svxPlane[SVXLADDERNUMBER];

  // Zero out the inputs
  error.setX(0.0);
  error.setY(0.0);
  error.setZ(0.0);

  // Fetch the geometry objects for SVX
  nSvxGeo = refGeo->get_svxGeo(arm, ilayer, svxPlane);

  // Loop over all geometry objects until there is an intersection
  itest = 0;
  igeo = 0;
  while (itest == 0 && igeo < nSvxGeo)
    {

      projectionFound = projectToPanel(svxPlane[igeo], intersection);
      if (projectionFound)
        itest = 1;
      igeo++;

    }

  return itest;
}

//   method to project the track to the crk
PHBoolean
PHLineTrack::projectToCrk(PHLine &intersection, PHPoint &error)
{
  short nCrkGeo;  // number of retrieved Crk geometry objects
  int igeo;       // geometry loop variable
  int itest;
  short numberOfIntersections;
  PHCylinderSection crk[2];
  
  // Zero out the inputs
  error.setX(0.0);
  error.setY(0.0);
  error.setZ(0.0);

  // Create the geometry objects for Crk

  // Fetch the geometry objects for Crk

  nCrkGeo = refGeo->get_crkGeo(arm, crk);
  // Loop over all geometry objects until there is an intersection
  itest = 0;
  igeo = 0;
  PHLine line1, line2;

  while (itest == 0 && igeo < nCrkGeo)
    {
      numberOfIntersections = intersectionLineCylinderSection(itsLine, 
							      crk[arm], 
							      line1, line2);
      if (numberOfIntersections)
        {
          intersection = line1;
          itest = 1;
        }
      igeo++;
    }

  return itest;
}

//   method to project the track to the TEC
PHBoolean
PHLineTrack::projectToTec(PHLine &intersection, PHPoint &error)
{
  PHBoolean itest = False;
  PHBoolean projectionFound = False;

  // Zero out the inputs
  error.setX(0.0);
  error.setY(0.0);
  error.setZ(0.0);

// Create Tec reference cylinder. Accept projections
// even if they miss the edge of Tec slightly (+- 10cm in Z).
// Accept intersection with negative X.

      double ref = TECREFERENCERADIUS;
      float Zwidth = (TECZWIDTH6+20.)/2.;
      PHPoint center(0.,0.,0.);
      PHVector axis(0.,0.,Zwidth);
      PHCylinder tecCylinder(center,ref,axis);

      PHLine intersection1;
      PHLine intersection2;

      projectionFound = projectToCylinder(tecCylinder, intersection1, intersection2);

      if(projectionFound) {
        if((intersection1.getBasepoint()).getX() < 0.) { 
             intersection = intersection1;
	     itest = True;
        }
        else if((intersection2.getBasepoint()).getX() < 0.) {
             intersection = intersection2;
             itest = True;
        }

      }

  return itest;
}

//   method to project the track to the TOF
PHBoolean
PHLineTrack::projectToTof(PHLine &intersection, PHPoint &error)
{
  short nTofGeo;  // number of retrieved TOF geometry objects
  int igeo;       // geometry loop variable
  PHBoolean itest;
  PHBoolean projectionFound;
  PHPanel tofPlane[TOFPANELSNUMBER];

  // Zero out the inputs
  error.setX(0.0);
  error.setY(0.0);
  error.setZ(0.0);

  // Create the geometry objects for TOF

  // Fetch the geometry objects for TOF
  nTofGeo = refGeo->get_tofGeo(arm, tofPlane);
  // Loop over all geometry objects until there is an intersection
  itest = 0;
  igeo = 0;
  while (itest == 0 && igeo < nTofGeo)
    {
      projectionFound = projectToPanel(tofPlane[igeo], intersection);
      if (projectionFound)
        {
	  itest = 1;
	}
      igeo++;
    }

  return itest;
}

//   method to project the track to the PbSc
PHBoolean
PHLineTrack::projectToPbSc(PHLine &intersection, PHPoint &error)
{
  short nPbscGeo;  // number of retrieved PBSC geometry objects
  int igeo;       // geometry loop variable
  PHBoolean itest;
  PHBoolean projectionFound;
  PHPanel pbscPlane[4];

  // Zero out the inputs
  error.setX(0.0);
  error.setY(0.0);
  error.setZ(0.0);

  // Create the geometry objects for PBSC

  // Fetch the geometry objects for PBSC
  nPbscGeo = refGeo->get_pbscGeo(arm, pbscPlane);

  // Loop over all geometry objects until there is an intersection
  itest = 0;
  igeo = 0;
  while (itest == 0 && igeo < nPbscGeo)
    {
      projectionFound = projectToPanel(pbscPlane[igeo], intersection);
      if (projectionFound)
        {
	  itest = 1;
	}
      igeo++;
    } 
  
  return itest;
}

//   method to project the track to the PbGl
PHBoolean
PHLineTrack::projectToPbGl(PHLine &intersection, PHPoint &error)
{
  short nPbglGeo;  // number of retrieved PBGL geometry objects
  int igeo;       // geometry loop variable
  PHBoolean itest;
  PHBoolean projectionFound;
  PHPanel pbglPlane[4];

  // Zero out the inputs
  error.setX(0.0);
  error.setY(0.0);
  error.setZ(0.0);

  // Create the geometry objects for PBGL

  // Fetch the geometry objects for PBGL
  nPbglGeo = refGeo->get_pbglGeo(arm, pbglPlane);

  // Loop over all geometry objects until there is an intersection
  itest = 0;
  igeo = 0;
  while (itest == 0 && igeo < nPbglGeo)
    {
      projectionFound = projectToPanel(pbglPlane[igeo], intersection);
      if (projectionFound)
        {
	  itest = 1;
	}
      igeo++;
    } 

  return itest;
}

//   method to project the track to the ACC
PHBoolean
PHLineTrack::projectToAcc(PHLine &intersection, PHPoint &error)
{
  short nAccGeo;  // number of retrieved TOF geometry objects
  int igeo;       // geometry loop variable
  PHBoolean itest;
  PHBoolean projectionFound;
  PHPanel accPlane[ACCPANELSNUMBER];

  // Zero out the inputs
  error.setX(0.0);
  error.setY(0.0);
  error.setZ(0.0);

  // Create the geometry objects for ACC

  // Fetch the geometry objects for ACC
  nAccGeo = refGeo->get_accGeo(arm, accPlane);
  // Loop over all geometry objects until there is an intersection
  itest = 0;
  igeo = 0;
  while (itest == 0 && igeo < nAccGeo)
    {
      projectionFound = projectToPanel(accPlane[igeo], intersection);
      if (projectionFound)
        {
	  itest = 1;
	}
      igeo++;
    }

  return itest;
}

PHBoolean
PHLineTrack::projectToTofw(PHLine &intersection, PHPoint &error)
{
  short nTofwGeo;  // number of retrieved TOFW geometry objects
  int igeo;       // geometry loop variable
  PHBoolean itest;
  PHBoolean projectionFound;
  PHPanel tofwPlane[TOFWPANELSNUMBER];

  // Zero out the inputs
  error.setX(0.0);
  error.setY(0.0);
  error.setZ(0.0);

  // Create the geometry objects for TOFW

  // Fetch the geometry objects for TOFW
  nTofwGeo = refGeo->get_tofwGeo(arm, tofwPlane);
  // Loop over all geometry objects until there is an intersection
  itest = 0;
  igeo = 0;
  while (itest == 0 && igeo < nTofwGeo)
    {
      projectionFound = projectToPanel(tofwPlane[igeo], intersection);
      if (projectionFound)
        {
	  itest = 1;
	}
      igeo++;
    }

  return itest;
}


//   method to project the track to the HBD 
PHBoolean
PHLineTrack::projectToHbd(PHLine &intersection, PHPoint &error)
{
  short nHbdGeo;  // number of retrieved HBD geometry objects
  int igeo;       // geometry loop variable
  PHBoolean itest;
  PHBoolean projectionFound;
  PHPanel hbdPlane[HBDPANELSNUMBER];

  // Zero out the inputs
  error.setX(0.0);
  error.setY(0.0);
  error.setZ(0.0);

  // Create the geometry objects for HBD

  // Fetch the geometry objects for HBD
  nHbdGeo = refGeo->get_hbdGeo(arm, hbdPlane);
  // Loop over all geometry objects until there is an intersection
  itest = 0;
  igeo = 0;
  while (itest == 0 && igeo < nHbdGeo)
    {
      projectionFound = projectToPanel(hbdPlane[igeo], intersection);
      if (projectionFound)
        {
	  itest = 1;
	}
      igeo++;
    }

  return itest;
}

//   method to project the track to a plane
PHBoolean
PHLineTrack::projectToPlane(const PHPlane &plane, PHLine &proj)
{
  return intersectionLinePlane(itsLine, plane, proj);
}

//   method to project the track to a panel
PHBoolean
PHLineTrack::projectToPanel(const PHPanel &panel, PHLine &proj)
{
  return intersectionLinePanel(itsLine, panel, proj);
}

//   method to project the track to a cylinder
short
PHLineTrack::projectToCylinder(const PHCylinder &cyl,
                               PHLine &proj1, PHLine &proj2)
{
  return intersectionLineCylinder(itsLine, cyl, proj1, proj2);
}

//   method to project the track to a sphere
short
PHLineTrack::projectToSphere(const PHSphere &sphere,
                             PHLine &proj1, PHLine &proj2)
{
  return intersectionLineSphere(itsLine, sphere, proj1, proj2);
}

// Predict the momentum
PHVector
PHLineTrack::predictMomentum()
{
  PHVector mom;

  mom.setX(0.0);
  mom.setY(0.0);
  mom.setZ(0.0);

  return mom;
}

// Calculate the path length to the CRK
double
PHLineTrack::pathLengthToCrk()
{
  return 0.0;
}

// Calculate the path length to the TOF
double
PHLineTrack::pathLengthToTof()
{

  double path;

  path = 0.0;

  PHBoolean TOFintersection;
  PHLine intersection,closest;
  PHPoint error;
  TOFintersection = projectToTof(intersection, error);
  if (TOFintersection)
    {
      projectToVertex(closest,error);
      PHVector theLength = (PHVector)(intersection.getBasepoint() - closest.getBasepoint());
      path = theLength.length();
    }
  else
    {
    }
  return path;
}

// Calculate the path length to the Emc
double
PHLineTrack::pathLengthToEmc()
{
  PHLine intersection,closest;
  PHPoint error;

  PHBoolean PbGlintersection = projectToPbGl(intersection, error);
  PHBoolean PbScintersection;

  PHPoint EMCPoint;
  if (PbGlintersection)
    {
      EMCPoint = intersection.getBasepoint();
    }
  else
    {
      PbScintersection = projectToPbSc(intersection, error);
      if (PbScintersection)
        {
          EMCPoint = intersection.getBasepoint();
        }
      else
        {
        }
    }
  projectToVertex(closest,error);
  PHVector theLength = (PHVector)(EMCPoint - closest.getBasepoint());
  double path = theLength.length();

  return path;
}


// Calculate the path length to the TOFW
double
PHLineTrack::pathLengthToTofw()
{

  double path = 0.0;
  PHLine intersection,closest;
  PHPoint error;

  if (projectToTofw(intersection, error))
    {
      projectToVertex(closest,error);
      PHVector theLength = (PHVector)(intersection.getBasepoint() - closest.getBasepoint());
      path = theLength.length();
    }
  else
    {
    }
  return path;
}


// Calculate the polyline for the track model
void
PHLineTrack::calcPolyLine()
{
  PHLine line;
  PHPoint closest;
  projectToVertex(line, closest);

  PHPolyLine poly;
  // the first point is the projection to the vertex
  poly.appendPoint(new PHPoint(closest));
  // one more point , somewhere far away along the line
  PHVector dir = line.getDirection();
  double length = 1000; // the polyline will end 10m away from the vertex
  PHVector endPoint = dir * length;
  PHPoint last = endPoint;
  PHPoint *ls = &last;
  poly.appendPoint(ls);
}

void
PHLineTrack::setLine( int iDch, dDchTracksWrapper * dchTrack )
{
  double xDch, yDch, zDch;
  double vxDch, vyDch, vzDch;
  // Fetch the drift chamber track reference point
  xDch = dchTrack->get_point(0, iDch);
  yDch = dchTrack->get_point(1, iDch);
  zDch = dchTrack->get_point(2, iDch);

  // Fetch the drift chamber track vector
  vxDch = dchTrack->get_direction(0, iDch);
  vyDch = dchTrack->get_direction(1, iDch);
  vzDch = dchTrack->get_direction(2, iDch);

  // Set up the reference point and line objects
  PHPoint dchPoint(xDch, yDch, zDch);
  PHVector dchVect(vxDch, vyDch, vzDch);
  
  // Store the reference line
  itsLine.setBasepoint(dchPoint);
  itsLine.setDirection(dchVect);
}


