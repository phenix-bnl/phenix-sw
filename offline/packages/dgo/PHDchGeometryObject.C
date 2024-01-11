// comment refers to previous update

// Implementation of class file: PHDchGeometryObject.h
// Created by: Federica Ceretto at Wed Feb 17 15:19:41 1999

//
//  Updated so that the high precision position refinements
//  (Card alpha, card tilt, beam offset) are fetchable
//  from external sources.
//
//                         TKH 1-21-2004
//

#include <PHDchGeometryObject.h>
#include <PHGeometry.h>
#include <PdbCoordinate.hh>
#include <PdbParameter.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <RunNumberRanges.h>
#include <recoConsts.h>


#include <cstdlib>
#include <cstring>
#include <fstream>
#include <string>

using namespace std;
using namespace PHGeometry;

//___________________________________________________________________________
PHDchGeometryObject::PHDchGeometryObject(PHDchAddressObject *add, short verb): 
  PHGeometryObject(add),
  dcEast(0),
  dcWest(0)
{
  initialize(add, verb);
  for (int i = 0; i<numberOfPlanes; i++) radiusAtExtremZ[i]=0;
}

//___________________________________________________________________________
PHDchGeometryObject::~PHDchGeometryObject( void )
{
  if( dcWest ) delete dcWest;
  if( dcEast ) delete dcEast;
}

//___________________________________________________________________________
void PHDchGeometryObject::initialize(PHDchAddressObject* add, short verb)
{
  verbose=verb;

  committingFlag = 0;
  dcWest = 0;
  dcEast = 0;
  address = add;
  dchaddress = add;
  initializeFlagXUV();
  commit();
}

//___________________________________________________________________________
PHBoolean PHDchGeometryObject::fetchFromFile()
{
  fetchGeometryInfoFromFile();
  fetchWirePositionsFromFile();
  fetchBeamsFromFile ();  // beams before frames...
  fetchFramesFromFile();
  fetchAlphasFromFile();
  fetchTiltsFromFile ();
  fetchUvttsFromFile ();

  calculateAverageRadius();
  initializeShapes();

  return True;
}

//___________________________________________________________________________
PHBoolean PHDchGeometryObject::setFileNames(const char* info, const char* wire, const char* frame, const char* alpha, const char* tilt, const char* beam, const char* uvtt)
{
  geoInfoFile  = info;
  geoWireFile  = wire;
  geoFrameFile = frame;
  geoAlphaFile = alpha;
  geoTiltFile  = tilt;
  geoBeamFile  = beam;
  geoUvttFile  = uvtt;

  if (!geoInfoFile  || !geoWireFile || !geoFrameFile || 
      !geoAlphaFile || !geoTiltFile || !geoBeamFile  || !geoUvttFile)
    {
      cout << "PHDchGeometryObject::no file name given !! Fatal !!" << endl;
      return False;
    }
  else
    {
      return True;
    }
}

//___________________________________________________________________________
void PHDchGeometryObject::initializeShapes()
{

  if( dcWest ) delete dcWest;
  if( dcEast ) delete dcEast;

  centerWest.setX(dcwestxo);
  centerWest.setY(dcwestyo);
  centerWest.setZ(dcwestzo);
  centerEast.setX(dceastxo);
  centerEast.setY(dceastyo);
  centerEast.setZ(dceastzo);

  double cylinderLengthWestArm = distancePointToPoint(getWireBasepointNorth(WEST , 0 , 0), getWireBasepointSouth(WEST, 0, 0));
  double cylinderLengthEastArm = distancePointToPoint(getWireBasepointNorth(EAST , 0 , 0), getWireBasepointSouth(EAST, 0, 0));

  axisWest.setX(dcwestxa*cylinderLengthWestArm / 2.);
  axisWest.setY(dcwestya*cylinderLengthWestArm / 2.);
  axisWest.setZ(dcwestza*cylinderLengthWestArm / 2.);

  axisEast.setX(dceastxa*cylinderLengthEastArm / 2.);
  axisEast.setY(dceastya*cylinderLengthEastArm / 2.);
  axisEast.setZ(dceastza*cylinderLengthEastArm / 2.);

  dcWest = new PHCylinderSection(centerWest, referenceRadius, axisWest);
  dcEast = new PHCylinderSection(centerEast, referenceRadius, axisEast);
  dcWest->setPhiRange(bottomPhiOfArm[WEST], topPhiOfArm[WEST]);
  dcEast->setPhiRange(topPhiOfArm[EAST], bottomPhiOfArm[EAST]);

  frames2MatrixAndVector(startingFrame[EAST], endingFrame[EAST], rotation[EAST], translation[EAST]);
  frames2MatrixAndVector(startingFrame[WEST], endingFrame[WEST], rotation[WEST], translation[WEST]);
}

float
PHDchGeometryObject::getDriftDistance(int plane) const
{
  float radius = averageRadius[plane];
  float distance = radius * deltaPhiOfCell / 2.0;
  return distance;
}

PHCylinderSection
PHDchGeometryObject::getCylinderSectionAtPlane(short arm, short plane)
{
  PHPoint center;
  PHVector axis;
  if (arm == (short)WEST)
    {
      center = centerWest;
      axis = axisWest * 1.1; // over dimensioned for Thomas k.H.
    }
  else
    {
      center = centerEast;
      axis = axisEast * 1.1; // over dimensioned for Thomas k.H.
    }
  double radius = getAverageRadius(plane);
  PHCylinderSection cylinderAtPlane(center, radius, axis);
  if (arm == (short)WEST)
    {
      cylinderAtPlane.setPhiRange(bottomPhiOfArm[WEST] - 10 / ToDegree, topPhiOfArm[WEST] + 10 / ToDegree);
    }
  else
    {
      cylinderAtPlane.setPhiRange(topPhiOfArm[EAST] - 10 / ToDegree , bottomPhiOfArm[EAST] + 10 / ToDegree);
    }

  return cylinderAtPlane;
}

PdbDchWire
PHDchGeometryObject::getWire(int arm, int plane, int cell) const
{
  PdbDchWire thisWire;
  thisWire.setNorthPoint(wireBasepointNorth[arm][plane][cell]);
  thisWire.setSouthPoint(wireBasepointSouth[arm][plane][cell]);
  thisWire.setNorthDrift(wireDriftDirectionNorth[arm][plane][cell]);
  thisWire.setSouthDrift(wireDriftDirectionSouth[arm][plane][cell]);
  return thisWire;
}

PdbDchWire
PHDchGeometryObject::getWire(PdbIndex* index) const
{
  int arm, plane, cell;
  dchaddress->setGlobalIndex(index);
  arm = dchaddress->getArm()->getValue();
  plane = dchaddress->getPlane()->getValue();
  cell = dchaddress->getCell()->getValue();
  
  return getWire(arm, plane, cell);
}

int
PHDchGeometryObject::findCorrespondingArm(const PHPoint& point)
{
  PHPoint pointInIdealFrame = point; //transformPoint(idealFrame, point, idealFrame); // need check

  double x = pointInIdealFrame.getX();
  if (x < 0)
    {
      return EAST;
    }
  else if (x > 0)
    {
      return WEST;
    }
  else
    {
      PHMessage("PHDchGeometryObject::findCorrespondingArm", PHError, "No ARM at x = 0 ");
      return -1;
    }
}

int
PHDchGeometryObject::findCorrespondingSide(const PHPoint& point)
{
  return (point.getZ() < 0.0) ? SOUTH : NORTH;
}

int
PHDchGeometryObject::findCorrespondingPlane(const PHPoint& point)
{
  PHPoint pointInIdealFrame = point;

  double radiusOfPoint = sqrt(pointInIdealFrame.getX() * pointInIdealFrame.getX() +
                              pointInIdealFrame.getY() * pointInIdealFrame.getY());

  int totalNumberOfPlanes = dchaddress->getPlane()->getMax() + 1;

  float distance;
  float minimumDistance = 10000;
  int planeOfMinimumDistance = -1;

  for (int i = 0; i < totalNumberOfPlanes; i++)
    {
      distance = fabs(radiusOfPoint - getAverageRadius(i)); // getAverageRadius ???
      if (distance < minimumDistance)
        {
          minimumDistance = distance;
          planeOfMinimumDistance = i;
        }
    }

  return planeOfMinimumDistance;
}

int
PHDchGeometryObject::getCellOfPoint(float phi, float alpha, float zed, int plane)
{
  int arm, cell;
  float phiMin, phiMax;

  if (phi > M_PI_2)
    {
      arm = EAST;
    }
  else
    {
      arm = WEST;
    }

  phiMin = bottomPhiOfArm[arm];
  phiMax = topPhiOfArm[arm];
  cell = (int)((phi - phiMin) * numberOfCells / (phiMax - phiMin));

  return cell;
}

int
PHDchGeometryObject::findCorrespondingCell(const PHPoint& point)
{
  int correspondingArm = findCorrespondingArm(point);
  int correspondingPlane = findCorrespondingPlane(point);
  int cellOfMinimumDistance = -1;
  float minimumDistance = 10000;
  float distance;

  PHPoint baseS, baseN;

  int totalNumberOfCells = dchaddress->getCell()->getMax() + 1;
  for (int i = 0; i < totalNumberOfCells; i++)
    {
      baseS = getWireBasepointSouth(correspondingArm, correspondingPlane, i);
      baseN = getWireBasepointNorth(correspondingArm, correspondingPlane, i);
      PHLine wireLine(baseN, (PHVector)(baseS - baseN));
      distance = distanceLinePoint(wireLine, point);
      if (distance < minimumDistance)
        {
          minimumDistance = distance;
          cellOfMinimumDistance = i;
        }
    }
  return cellOfMinimumDistance;
}

PHBoolean
PHDchGeometryObject::getIntersectionTrackWirePlane(const PHLine& trackLine, int planeId, int armId, PHPoint& inter)
{
  // restriction at ideal cylinder
  PHPoint point1;
  PHPoint point2;
  PHPoint center(0., 0., 0.);
  PHPoint null;
  PHVector axis(0., 0., getLengthOfXWire(1) / 2.);

  PHCylinderSection cylinderSection(center, averageRadius[planeId], axis);
  if (armId == (short)EAST)
    { //  EAST
      cylinderSection.setPhiRange(topPhiOfArm[EAST], bottomPhiOfArm[EAST]);  // ??? temporary
    }
  else if (armId == (short)WEST)
    {
      cylinderSection.setPhiRange(bottomPhiOfArm[WEST], topPhiOfArm[WEST]);  // ??? temporary
    }

  PHBoolean flag = intersectionLineCylinderSection(trackLine, cylinderSection, point1, point2);
  if (!flag )
    {
      inter = null;
    }
  else
    {
      inter = point1;
    }
  return flag;
}

PHVector
PHDchGeometryObject::getLocalDriftDirection(int arm, int plane, int cell, float z) const
{
  // written by Axel 5-18-00 (using approximations !!!)
  PHVector driftNorth = wireDriftDirectionNorth[arm][plane][cell];
  PHVector driftSouth = wireDriftDirectionSouth[arm][plane][cell];
  float fraction = (1 - z / wireBasepointNorth[arm][plane][cell].getZ()) / 2. ;
  PHVector driftDirection = (driftNorth + (driftSouth - driftNorth) * fraction);
  
  return driftDirection;
}

PHLine
PHDchGeometryObject::getWireAsLine(short a, short p, short c)
{
  PHLine line;
  line.setBasepoint(wireBasepointNorth[a][p][c]);
  line.setDirection(PHVector(wireBasepointSouth[a][p][c] - wireBasepointNorth[a][p][c]));
  return line;
}

PHBoolean
PHDchGeometryObject::expectSignalFrom(short arm, short plane, short cell, PHPoint point)
{
  // need checks for Cosmic and MonteCarlo
  double zCoordinate = point.getZ();

  double fraction = (zCoordinate - (wireBasepointNorth[arm][plane][cell]).getZ()) / getLengthOfXWire(1); // ?????
  PHPoint pointOnWire = (wireBasepointNorth[arm][plane][cell]) +
                        (wireBasepointSouth[arm][plane][cell] - wireBasepointNorth[arm][plane][cell]) * fraction;
  PHVector driftOnWire = (wireDriftDirectionNorth[arm][plane][cell]) +
                         (wireDriftDirectionSouth[arm][plane][cell] - wireDriftDirectionNorth[arm][plane][cell]) * fraction;

  PHPoint pointTest = pointOnWire + (PHPoint)(driftOnWire);

  PHCylPoint cylPointTrack = point;
  PHCylPoint cylPointWire = pointOnWire;
  PHCylPoint cylPointTest = pointTest;

  double PhiWire = (double)cylPointWire.getPhi();
  double PhiPunch = (double)cylPointTrack.getPhi();
  double PhiTest = (double)cylPointTest.getPhi();

  if (PhiWire < -M_PI_2)
    PhiWire = PhiWire + 2 * M_PI;
  if (PhiPunch < -M_PI_2)
    PhiPunch = PhiPunch + 2 * M_PI;
  if (PhiTest < -M_PI_2)
    PhiTest = PhiTest + 2 * M_PI;

  if ((PhiTest > PhiWire && PhiPunch > PhiWire) || (PhiTest < PhiWire && PhiPunch < PhiWire))
    {
      return True;
    }
  else
    {
      return False;
    }
}

void
PHDchGeometryObject::printWire(PdbIndex* index) const
  {
    int arm, plane, cell;
    dchaddress->setGlobalIndex(index);
    arm = dchaddress->getArm()->getValue();
    plane = dchaddress->getPlane()->getValue();
    cell = dchaddress->getCell()->getValue();
    if (verbose)
      {
        cout << "ARM  : " << arm << endl;
        cout << "PLANE: " << plane << endl;
        cout << "CELL : " << cell << endl;
      }
    wireBasepointNorth[arm][plane][cell].print();
    wireBasepointSouth[arm][plane][cell].print();
    wireDriftDirectionNorth[arm][plane][cell].print();
    wireDriftDirectionSouth[arm][plane][cell].print();

    PHVector Wire(wireBasepointNorth[arm][plane][cell]-wireBasepointSouth[arm][plane][cell]);
    PHVector Zed(0,0,1);
    float ang = angle(Wire,Zed);
    cout << "angle of wire = " << ang*180.0/M_PI << endl;

  }

PHBoolean
PHDchGeometryObject::update(PHTimeStamp &Tstart, PHTimeStamp &Tstop,
                            const char *calibname, PdbBankID bankID, const char*descrip )
{
  //-----------------------------------------------------------------
  // This function updates in the database both the Geometry Info and the Wire positions
  // For detailed explanation see the corresponding functions: updateGeometryInfo() and updateWirePositions()
  //-----------------------------------------------------------------
  char genName  [50];
  char infoName [50];
  char wireName [50];
  char frameName[50];
  char alphaName[50];
  char tiltName [50];
  char beamName [50];
  char uvttName [50];

  strcpy(genName,  calibname);
  strcpy(infoName, genName);
  strcpy(wireName, genName);
  strcpy(frameName,genName);
  strcpy(alphaName,genName);
  strcpy(tiltName, genName);
  strcpy(beamName, genName);
  strcpy(uvttName, genName);

  strcat(infoName,  "info");
  strcat(wireName,  "wire");
  strcat(frameName, "frame");
  strcat(alphaName, "alpha");
  strcat(tiltName,  "tilt");
  strcat(beamName,  "beam");
  strcat(uvttName,  "uvtt");

  if (committed == 1)
    {
      if (!application->startUpdate())
        {
          PHMessage("PHDchGeometryObject", PHError, "Aborting ... Database not writable");
          application->abort();
        }
      else
        {
          committed = 0;
        }
    }


  start = Tstart;
  stop = Tstop;
  if (committingFlag == 0 || committingFlag == 1)
    {
      infoBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, infoName);
      updateGeometryInfo();
    }
  if (committingFlag == 0 || committingFlag == 2)
    {
      geometryBank = bankManager->createBank("PdbDchWireBank", bankID, descrip, Tstart, Tstop, wireName);
      updateWirePositions();
    }
  if (committingFlag == 0 || committingFlag == 3)
    {
      frameBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, frameName);
      updateFrames();
    }
  if (committingFlag == 0 || committingFlag == 4)
    {
      alphaBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, alphaName);
      updateAlphas();
    }
  if (committingFlag == 0 || committingFlag == 5)
    {
      tiltBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, tiltName);
      updateTilts();
    }
  if (committingFlag == 0 || committingFlag == 6)
    {
      beamBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, beamName);
      updateBeams();
    }
  if (committingFlag == 0 || committingFlag == 7)
    {
      uvttBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, uvttName);
      updateUvtts();
    }

  return False;
}

PHBoolean
PHDchGeometryObject::updateWirePositions()

{
  // This function will be used for writing for the first time
  // in the DB the MonteCarlo informations and/or
  // the survey measurements (if any).
  // For the moment, the geometry in form of coordinated are read in from a File (using the initialize function) !
  // If needed every sub-system should define its own format

  int length = (int)(dchaddress->getGlobalIndex()->getNumberOf() / 2.0); // only one entry per full wire (N to S)
  geometryBank->setLength(length);     // the length of the bank is obtained from the address class

  int arm, plane, cell;
  PdbDchWire *wire;

  for (int i = 0; i < length; i++)
    {
      dchaddress->getGlobalIndex()->setValue(i);                  // set the value of the global index
      dchaddress->setGlobalIndex(dchaddress->getGlobalIndex());
      arm = dchaddress->getArm()->getValue();
      plane = dchaddress->getPlane()->getValue();
      cell = dchaddress->getCell()->getValue();

      wire = (PdbDchWire*) & geometryBank->getEntry(i);
      wire->setNorthPoint(wireBasepointNorth[arm][plane][cell]);
      wire->setSouthPoint(wireBasepointSouth[arm][plane][cell]);
      wire->setNorthDrift(wireDriftDirectionNorth[arm][plane][cell]);
      wire->setSouthDrift(wireDriftDirectionSouth[arm][plane][cell]);
    }
  return True;
}

PHBoolean
PHDchGeometryObject::updateGeometryInfo()

{ //-------------------------------------------------------------------------
  // At present this function introduces in the database the quantities
  // previously written in the DchGeometry  STAF table
  //------------------------------------------------------------------------

  int length = 97 + 12;               // total number of  values from old Staf table + 2 + cylinder sectins
  infoBank->setLength(length);

  PdbParameter *parameter;

  for (int i = 0; i < length; i++)
    {
      parameter = (PdbParameter*) & infoBank->getEntry(i);
      parameter->setParameter(tmpValue[i]);
      parameter->setName(parameterName[i].c_str());
    }

  return True;
}

PHBoolean
PHDchGeometryObject::updateFrames()

{
  int length = 12 + 12;
  frameBank->setLength(length);

  PdbParameter *parameter;

  for (int i = 0; i < length; i++)
    {
      parameter = (PdbParameter*) & frameBank->getEntry(i);
      parameter->setParameter(tmpValueFrame[i]);
      parameter->setName("");
      parameter->print();
    }

  return True;
}

PHBoolean
PHDchGeometryObject::updateAlphas()
{
  //  OK gang...now it gets intense.
  //  In this routine we will be _storing_ the entire bank
  //  of alpha calibration parameters...YEE HAW!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  \  entries == number of entries that *SHOULD* follow
  //
  //      /   set   (X1 or X2)
  //  b  |    arm
  //  o  |    side
  //  d  |    cell
  //  y   \   alpha
  //
  //  The number of times the body repeats will be 2*2*2*80
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 1-21-2004
  //

  int length = 5*numberOfWireTypes*numberOfArms*numberOfSides*numberOfCells + 2; // array + 2 hdr
  alphaBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
  parameter->setParameter(length-2);
  parameter->setName("entries");

  for (int i=0; i<numberOfWireTypes; i++) {
    for (int j=0; j<numberOfArms; j++) {
      for (int k=0; k<numberOfSides; k++) {
	for (int l=0; l<numberOfCells; l++) {
	  
	  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
	  parameter->setParameter(i);
	  parameter->setName("WireType");

	  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
	  parameter->setParameter(j);
	  parameter->setName("Arm");

	  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
	  parameter->setParameter(k);
	  parameter->setName("Side");

	  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
	  parameter->setParameter(l);
	  parameter->setName("Cell");

	  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
	  parameter->setParameter(A0[i][j][k][l]);
	  parameter->setName("Alpha");

	}
      }
    }
  }

  return True;
}

PHBoolean
PHDchGeometryObject::updateTilts()
{
  //  OK gang...now it gets intense.
  //  In this routine we will be _storing_ the entire bank
  //  of tilt calibration parameters...YEE HAW!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  \  entries == number of entries that *SHOULD* follow
  //
  //      /   arm
  //  b  |    cell
  //  o  |    x1Slope
  //  d  |    x1Offset
  //  y  |    x2Slope
  //      \   x2Offset
  //
  //  The number of times the body repeats will be 2*80
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 1-21-2004
  //

  int length = 6*numberOfArms*numberOfCells + 2; // array + 2 hdr
  tiltBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *)& tiltBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *)& tiltBank->getEntry(index++);
  parameter->setParameter(length-2);
  parameter->setName("entries");

  for (int i=0; i<numberOfArms; i++) {
    for (int j=0; j<numberOfCells; j++) {
      
      parameter = (PdbParameter *)& tiltBank->getEntry(index++);
      parameter->setParameter(i);
      parameter->setName("Arm");
      
      parameter = (PdbParameter *)& tiltBank->getEntry(index++);
      parameter->setParameter(j);
      parameter->setName("Cell");
      
      parameter = (PdbParameter *)& tiltBank->getEntry(index++);
      parameter->setParameter(x1Slope[i][j]);
      parameter->setName("x1Slope");
      
      parameter = (PdbParameter *)& tiltBank->getEntry(index++);
      parameter->setParameter(x1Offset[i][j]);
      parameter->setName("x1Offset");
      
      parameter = (PdbParameter *)& tiltBank->getEntry(index++);
      parameter->setParameter(x2Slope[i][j]);
      parameter->setName("x2Slope");
      
      parameter = (PdbParameter *)& tiltBank->getEntry(index++);
      parameter->setParameter(x2Offset[i][j]);
      parameter->setName("x2Offset");
      
    }
  }
  
  return True;
}

PHBoolean
PHDchGeometryObject::updateBeams()
{
  //  OK gang...now it gets intense.
  //  In this routine we will be _storing_ the entire bank
  //  of beam calibration parameters...YEE HAW!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  \  entries == number of entries that *SHOULD* follow
  //
  //  b  /  xBeamOffset 
  //  d  \  yBeamOffset
  //
  //  The number of times the body repeats will be 1
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 1-21-2004
  //
  //  This will now be updated for TWO beam offsets.
  //  The two beam offsets are x,y in east and x,y in west.
  //  In order to not disturb the old code, we will
  //  store these new values as "scheme 2"
  //
  //  h  /  scheme  == 2 indicates east and west independent...
  //  d  \  entries == number of entries that *SHOULD* follow
  //
  //  b  /  xBeamOffsetEast
  //  o |   yBeamOffsetEast 
  //  d |   xBeamOffsetWest 
  //  y  \  yBeamOffsetWest
  //
  //                         TKH RP 3-16-2009
  //

  int length = 6; // array of 4 data words + 2 hdr words
  beamBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *)& beamBank->getEntry(index++);
  parameter->setParameter(2.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *)& beamBank->getEntry(index++);
  parameter->setParameter(length-2);
  parameter->setName("entries");

  parameter = (PdbParameter *)& beamBank->getEntry(index++);
  parameter->setParameter(xBeamOffsetEast);
  parameter->setName("xBeamOffsetEast");

  parameter = (PdbParameter *)& beamBank->getEntry(index++);
  parameter->setParameter(yBeamOffsetEast);
  parameter->setName("yBeamOffsetEast");

  parameter = (PdbParameter *)& beamBank->getEntry(index++);
  parameter->setParameter(xBeamOffsetWest);
  parameter->setName("xBeamOffsetWest");

  parameter = (PdbParameter *)& beamBank->getEntry(index++);
  parameter->setParameter(yBeamOffsetWest);
  parameter->setName("yBeamOffsetWest");

  
  return True;
}

PHBoolean
PHDchGeometryObject::updateUvtts()
{
  //  OK gang...now it gets intense.
  //  In this routine we will be _storing_ the entire bank
  //  of uvtt calibration parameters...YEE HAW!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  \  entries == number of entries that *SHOULD* follow
  //
  //  b  /   arm 
  //  o  |   wire
  //  d  |   slope
  //  y  \   offset
  //
  //  The number of times the body repeats will be 2*16
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 1-21-2004
  //

  int length = 4*numberOfArms*16 + 2; // array + 2 hdr
  uvttBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *)& uvttBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *)& uvttBank->getEntry(index++);
  parameter->setParameter(length-2);
  parameter->setName("entries");

  for (int i=0; i<numberOfArms; i++) {
    for (int j=0; j<16; j++) {
      
      parameter = (PdbParameter *)& uvttBank->getEntry(index++);
      parameter->setParameter(i);
      parameter->setName("Arm");
      
      parameter = (PdbParameter *)& uvttBank->getEntry(index++);
      parameter->setParameter(j);
      parameter->setName("Wire");
      
      parameter = (PdbParameter *)& uvttBank->getEntry(index++);
      parameter->setParameter(uvSlope[i][j]);
      parameter->setName("uvSlope");
      
      parameter = (PdbParameter *)& uvttBank->getEntry(index++);
      parameter->setParameter(uvOffset[i][j]);
      parameter->setName("uvOffset");
      
      
    }
  }
  
  return True;
}

PHBoolean
PHDchGeometryObject::updateReferenceFrame(PHTimeStamp &Tstart, PHTimeStamp &Tstop,
    const char *calibname, PdbBankID bankID, const char*descrip )
{
  if (committed == 1)
    {
      if (!application->startUpdate())
        {
          PHMessage("PHDchGeometryObject", PHError, "Aborting ... Database not writable");
          application->abort();
        }
      else
        {
          committed = 0;
        }
    }

  frameBank = bankManager->createBank("PdbCoordinateBank", bankID, descrip, Tstart, Tstop, calibname);

  int numberOfFrames = 1;
  int length = 3 * numberOfFrames;
  int translationId = 0;
  int firstVectorId = 1;
  int secondVectorId = 2;

  frameBank->setLength(length);

  PdbCoordinate *coordinate;
  for (int i = 0; i < (int)frameBank->getLength(); i++)
    {
      coordinate = (PdbCoordinate*) & frameBank->getEntry(i);
      if (i == translationId)
        {
          coordinate->setParameter(0, 0); //
          coordinate->setParameter(1, 0); //
          coordinate->setParameter(2, 0); //
        }
      else if (i == firstVectorId)
        {
          coordinate->setParameter(0, 1); //
          coordinate->setParameter(1, 0); //
          coordinate->setParameter(2, 0); //
        }
      else if (i == secondVectorId)
        {
          coordinate->setParameter(0, 0); //
          coordinate->setParameter(1, 1); //
          coordinate->setParameter(2, 0); //
        }
    }

  return True;
}

PHPoint
PHDchGeometryObject::rotateAndTranslate(short arm, PHPoint point)
{
  return transformPoint(rotation[arm], translation[arm], point);
}

PHVector
PHDchGeometryObject::rotateAndTranslate(short arm, PHVector vector)
{
  return transformVector(rotation[arm], vector);
}

PHPoint
PHDchGeometryObject::rotateAndTranslateInverse(short arm, PHPoint point)
{
  return transformPoint(rotationInverse[arm], translationInverse[arm], point);
}

PHVector
PHDchGeometryObject::rotateAndTranslateInverse(short arm, PHVector vector)
{
  return transformVector(rotationInverse[arm], vector);
}

PHBoolean
PHDchGeometryObject::rotateAndTranslate(PHFrame initialE, PHFrame finalE, PHFrame initialW, PHFrame finalW)
{
  PHMessage("PHDchGeometryObject::rotateAndTranslate(PHFrame...) ", PHWarning,
            "The Initial frames should be simmetry frames of the DRIFT CHAMBER");

  // the initial frame is the trivial one (Simmetry frames of the Drift Chamber ;
  startingFrame[EAST] = initialE;
  startingFrame[WEST] = initialW;
  endingFrame[EAST] = finalE;
  endingFrame[WEST] = finalW;

  rotateAndTranslate();

  return True;
}

PHBoolean
PHDchGeometryObject::rotateAndTranslate()
{
  short arm, plane, cell;
  short minarm, minplane, mincell;
  PHPoint northPoint, southPoint;
  PHVector northDrift, southDrift;

  minarm = dchaddress->getArm()->getMin();
  minplane = dchaddress->getPlane()->getMin();
  mincell = dchaddress->getCell()->getMin();

  frames2MatrixAndVector(startingFrame[EAST], endingFrame[EAST],
                         rotation[EAST], translation[EAST]);
  frames2MatrixAndVector(startingFrame[WEST], endingFrame[WEST],
                         rotation[WEST], translation[WEST]);
  frames2MatrixAndVector(endingFrame[EAST],
                         startingFrame[EAST],
                         rotationInverse[EAST],
                         translationInverse[EAST]);
  frames2MatrixAndVector(endingFrame[WEST],
                         startingFrame[WEST],
                         rotationInverse[WEST],
                         translationInverse[WEST]);

  for (arm = minarm; arm < numberOfArms; arm++)
    {
      for (plane = minplane; plane < numberOfPlanes; plane++)
        {
          for (cell = mincell; cell < numberOfCells; cell++)
            {
              northPoint = wireBasepointNorth[arm][plane][cell];
              southPoint = wireBasepointSouth[arm][plane][cell];
              northDrift = wireDriftDirectionNorth[arm][plane][cell];
              southDrift = wireDriftDirectionSouth[arm][plane][cell];

              wireBasepointNorth[arm][plane][cell] =
                transformPoint(rotation[arm],
                               translation[arm],
                               northPoint);
              wireBasepointSouth[arm][plane][cell] =
                transformPoint(rotation[arm],
                               translation[arm],
                               southPoint);
              wireDriftDirectionNorth[arm][plane][cell] =
                transformVector(rotation[arm], northDrift);
              wireDriftDirectionSouth[arm][plane][cell] =
                transformVector(rotation[arm], southDrift);
            }
        }
    }

  *dcWest = transformCylinderSection(rotation[WEST],
                                     translation[WEST],
                                     *dcWest);
  *dcEast = transformCylinderSection(rotation[EAST],
                                     translation[EAST],
                                     *dcEast);

  centerWest = dcWest->getCenter();
  centerEast = dcEast->getCenter();
  axisWest = dcWest->getAxis();
  axisEast = dcEast->getAxis();

  return True;
}

PHBoolean
PHDchGeometryObject::rotateAndTranslate (PHTimeStamp &Tsearch,
    const char *calibname,
    PdbBankID bankID)
{
  if (committed == 1)
    {
      if (!application->startRead())
        {
          PHMessage("PHDchGeometryObject", PHError,
                    "Aborting ... Database not readable");
          application->abort();
        }
      else
        {
          committed = 0;
        }
    }

  PHVector vector[12];
  frameBank = bankManager->fetchBank("PdbCoordinateBank", bankID,
                                     calibname, Tsearch);

  PdbCoordinate *pointVector;
  for (unsigned int i = 0; i < frameBank->getLength(); i++)
    {
      pointVector = (PdbCoordinate*) & frameBank->getEntry(i);
      vector[i].setX(pointVector->getParameter(0));
      vector[i].setY(pointVector->getParameter(1));
      vector[i].setZ(pointVector->getParameter(2));
    }
  // I know it looks ugly but for now it's like that !!!
  PHFrame frames[2][2];
  for ( int k = 0; k < 12 ; k = k + 3)
    {
      PHFrame tmp(vector[k], vector[k + 1], vector[k + 2]);
      if (k == 0)
        {
          frames[EAST][0] = tmp;
        }
      else if (k == 3)
        {
          frames[EAST][1] = tmp;
        }
      else if (k == 7)
        {
          frames[WEST][0] = tmp;
        }
      else
        {
          frames[WEST][1] = tmp;
        }
    }

  rotateAndTranslate(frames[EAST][0], frames[EAST][1], frames[WEST][0], frames[WEST][1]);

  return True;
}

PHBoolean
PHDchGeometryObject::updateValidityTimeForLastBank(PHTimeStamp& Tstart, PHTimeStamp& Tstop,
    PHTimeStamp& Tsearch, const char *calibname, PdbBankID bankID, int force )
{
  if (!fetch(Tsearch, calibname, bankID))
    return False;
  if (stop == PHTimeStamp::PHFarFuture || force)
    {
      infoBank->setEndValTime(Tstop);
      geometryBank->setEndValTime(Tstop);
      frameBank->setEndValTime(Tstop);
      infoBank->setStartValTime(Tstart);
      geometryBank->setStartValTime(Tstart);
      frameBank->setStartValTime(Tstart);

      stop = Tstop;
    }
  else
    {
      PHMessage("PHDchGeometry::updateValidityTimeForLastBank", PHWarning, "Tstop not  in Far Future");
    }
  return True;
}

PHBoolean
PHDchGeometryObject::fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID bankID )
{
  char genName  [50];
  char infoName [50];
  char wireName [50];
  char frameName[50];
  char alphaName[50];
  char tiltName [50];
  char beamName [50];
  char uvttName [50];

  strcpy(genName,   calibname);
  strcpy(infoName,  genName);
  strcpy(wireName,  genName);
  strcpy(frameName, genName);
  strcpy(alphaName, genName);
  strcpy(tiltName,  genName);
  strcpy(beamName,  genName);
  strcpy(uvttName,  genName);

  strcat(infoName,  "info");
  strcat(wireName,  "wire");
  strcat(frameName, "frame");
  strcat(alphaName, "alpha");
  strcat(tiltName,  "tilt");
  strcat(beamName,  "beam");
  strcat(uvttName,  "uvtt");

  if (committed == 1)
    {
      if (!application->startRead())
        {
          PHMessage("PHDchGeometryObject", PHError, "Aborting ... Database not readable");
          application->abort();
        }
      else
        {
          committed = 0;
        }
    }

  infoBank     = bankManager->fetchBank("PdbParameterBank", bankID, infoName,  Tsearch);
  geometryBank = bankManager->fetchBank("PdbDchWireBank",   bankID, wireName,  Tsearch);
  frameBank    = bankManager->fetchBank("PdbParameterBank", bankID, frameName, Tsearch);
  alphaBank    = bankManager->fetchBank("PdbParameterBank", bankID, alphaName, Tsearch);
  tiltBank     = bankManager->fetchBank("PdbParameterBank", bankID, tiltName,  Tsearch);
  beamBank     = bankManager->fetchBank("PdbParameterBank", bankID, beamName,  Tsearch);
  uvttBank     = bankManager->fetchBank("PdbParameterBank", bankID, uvttName,  Tsearch);
  parameterName.clear();

  if (!frameBank || !geometryBank || !infoBank || !alphaBank || !tiltBank || !beamBank || !uvttBank)
    return False;
  start = geometryBank->getStartValTime();
  stop = geometryBank->getEndValTime();
  
  cout << "You have requested DRIFT CHAMBER calibrations for the date:  " << Tsearch <<endl;

  cout << "Dch Info valid from:" << infoBank->getStartValTime();
  cout << " until " << infoBank->getEndValTime();
  cout << " inserted " << infoBank->getInsertTime() << endl;

  cout << "Dch Geom valid from:" << geometryBank->getStartValTime();
  cout << " until " << geometryBank->getEndValTime();
  cout << " and inserted " << geometryBank->getInsertTime() << endl;

  cout << "Dch fram valid from:" << frameBank->getStartValTime();
  cout << " until " << frameBank->getEndValTime();
  cout << " and inserted " << frameBank->getInsertTime() << endl;

  cout << "Dch alph valid from:" << alphaBank->getStartValTime();
  cout << " until " << alphaBank->getEndValTime();
  cout << " and inserted " << alphaBank->getInsertTime() << endl;

  cout << "Dch tilt valid from:" << tiltBank->getStartValTime();
  cout << " until " << tiltBank->getEndValTime();
  cout << " and inserted " << tiltBank->getInsertTime() << endl;

  cout << "Dch UVtt valid from:" << uvttBank->getStartValTime();
  cout << " until " << uvttBank->getEndValTime();
  cout << " and inserted " << uvttBank->getInsertTime() << endl;

  fetchGeometryInfo();
  fetchWirePositions();
  fetchBeams ();  // beams before frames...
  fetchFrames();
  fetchAlphas();
  fetchTilts ();
  fetchUvtts ();

  calculateAverageRadius();
  initializeShapes();
  delete frameBank;
  frameBank = 0;
  delete  geometryBank;
  geometryBank = 0;
  delete infoBank;
  infoBank = 0;
  delete alphaBank;
  alphaBank = 0;
  delete tiltBank;
  tiltBank = 0;
  delete beamBank;
  beamBank = 0;
  delete uvttBank;
  uvttBank = 0;
  return True;
}

PHBoolean
PHDchGeometryObject::fetchFrames()
{

  string parName;
  PdbParameter *parameter;

  parameterFrame.clear();
  for (unsigned int i = 0; i < frameBank->getLength(); i++)
    {
      parameter = (PdbParameter*) & frameBank->getEntry(i);
      tmpValueFrame[i] = parameter->getParameter();
      parName = parameter->getName();
      parameterFrame.push_back(parName);
    }

  int m = 0;
  double oxe = tmpValueFrame[m++];  // origin X
  double oye = tmpValueFrame[m++];
  double oze = tmpValueFrame[m++];
  double a1xe = tmpValueFrame[m++];
  double a1ye = tmpValueFrame[m++];
  double a1ze = tmpValueFrame[m++];
  double a2xe = tmpValueFrame[m++];
  double a2ye = tmpValueFrame[m++];
  double a2ze = tmpValueFrame[m++];
  double a3xe = tmpValueFrame[m++];
  double a3ye = tmpValueFrame[m++];
  double a3ze = tmpValueFrame[m++];

  double oxw = tmpValueFrame[m++];
  double oyw = tmpValueFrame[m++];
  double ozw = tmpValueFrame[m++];
  double a1xw = tmpValueFrame[m++];
  double a1yw = tmpValueFrame[m++];
  double a1zw = tmpValueFrame[m++];
  double a2xw = tmpValueFrame[m++];
  double a2yw = tmpValueFrame[m++];
  double a2zw = tmpValueFrame[m++];
  double a3xw = tmpValueFrame[m++];
  double a3yw = tmpValueFrame[m++];
  double a3zw = tmpValueFrame[m++];



  /////////////////////////////////////
  ////
  /////////////////////////////////////

  PHPoint originE(oxe, oye, oze);
  PHPoint originW(oxw, oyw, ozw);

  // First order offset correction for " beam position shift " 

  recoConsts *rc = recoConsts::instance();
  if (rc->FlagExist("RUNNUMBER"))
    {
      int run = rc->get_IntFlag("RUNNUMBER");
      if (run > (int) BEGIN_OF_RUN3 && run < (int) BEGIN_OF_RUN5)  // We don't hard code starting in Run5.
	{
	  double xy[4];
	  GetBeamPosition(run, xy);

	  double x = originE.getX();
	  double y = originE.getY();
	  originE.setX(x + 0.124221 + 0.032527 - xy[0]);
	  originE.setY(y + 0.006808 + 0.013707 - xy[1]);

	  x = originW.getX();
	  y = originW.getY();
	  originW.setX(x - 0.469652 + 0.053077 - xy[2]);
	  originW.setY(y + 0.025964 + 0.046085 - xy[3]);
	}
      if (run >= (int) BEGIN_OF_RUN5)
	{
	  double xy[4];
	  GetBeamPosition(run, xy);

	  double x = originE.getX();
	  double y = originE.getY();
	  originE.setX(x - xy[0]);
	  originE.setY(y - xy[1]);

	  x = originW.getX();
	  y = originW.getY();
	  originW.setX(x - xy[2]);
	  originW.setY(y - xy[3]);
	}
    }
  ////////////////////////////////////
  //
  ////////////////////////////////////

  PHVector uE(a1xe, a1ye, a1ze);
  PHVector vE(a2xe, a2ye, a2ze);
  PHVector wE(a3xe, a3ye, a3ze);

  PHVector uW(a1xw, a1yw, a1zw);
  PHVector vW(a2xw, a2yw, a2zw);
  PHVector wW(a3xw, a3yw, a3zw);

  uE.normalize();
  vE.normalize();
  wE.normalize();

  uW.normalize();
  vW.normalize();
  wW.normalize();

  PHVector v1;
  PHVector v2;

  v1 = uE.cross(vE);
  v1.normalize();
  v2 = v1.cross(uE);
  v2.normalize();

  vE = v2;
  wE = uE.cross(vE);
  v1 = uW.cross(vW);
  v1.normalize();
  v2 = v1.cross(uW);
  v2.normalize();

  vW = v2;
  wW = uW.cross(vW);
  PHFrame initialE;
  PHFrame initialW;

  PHFrame finalE(originE, uE, vE);
  PHFrame finalW(originW, uW, vW);

  startingFrame[EAST] = initialE;
  startingFrame[WEST] = initialW;
  endingFrame[EAST] = finalE;
  endingFrame[WEST] = finalW;

  return True;
}

PHBoolean
PHDchGeometryObject::fetchGeometryInfo()
{
  const char* parName;
  PdbParameter *parameter;

  for (unsigned int i = 0; i < infoBank->getLength(); i++)
    {
      parameter = (PdbParameter*) & infoBank->getEntry(i);
      tmpValue[i] = parameter->getParameter();
      parName = parameter->getName();
      parameterName.push_back(parName);
    }

  int m = 0;
  ncells = tmpValue[m++];
  ngusset = tmpValue[m++];
  ti_switch = tmpValue[m++];
  suppzlength = tmpValue[m++];
  innerRadius = tmpValue[m++];
  outerRadius = tmpValue[m++];
  phibotw = tmpValue[m++];
  phitopw = tmpValue[m++];
  phitope = tmpValue[m++];
  phibote = tmpValue[m++];
  planethick = tmpValue[m++];
  uvangle = tmpValue[m++];
  winthickin = tmpValue[m++];
  winthickout = tmpValue[m++];
  supptiside = tmpValue[m++];
  suppalside = tmpValue[m++];
  suppzthick = tmpValue[m++];
  supptibase = tmpValue[m++];
  suppalbase = tmpValue[m++];
  x1baserad = tmpValue[m++];
  x2baserad = tmpValue[m++];
  x1basez = tmpValue[m++];
  x2basez = tmpValue[m++];
  x1slotthick = tmpValue[m++];
  x2slotthick = tmpValue[m++];
  x1slotz = tmpValue[m++];
  x2slotz = tmpValue[m++];
  x1suppthick = tmpValue[m++];
  x2suppthick = tmpValue[m++];
  x1suppz = tmpValue[m++];
  x2suppz = tmpValue[m++];
  x1rextent = tmpValue[m++];
  x2rextent = tmpValue[m++];
  u1rextent = tmpValue[m++];
  v1rextent = tmpValue[m++];
  u2rextent = tmpValue[m++];
  v2rextent = tmpValue[m++];
  u1basez = tmpValue[m++];
  v1basez = tmpValue[m++];
  u2basez = tmpValue[m++];
  v2basez = tmpValue[m++];
  u1slotz = tmpValue[m++];
  v1slotz = tmpValue[m++];
  u2slotz = tmpValue[m++];
  v2slotz = tmpValue[m++];
  u1suppz = tmpValue[m++];
  v1suppz = tmpValue[m++];
  u2suppz = tmpValue[m++];
  v2suppz = tmpValue[m++];
  cfibinrad = tmpValue[m++];
  cfiboutrad = tmpValue[m++];
  propregwidth = tmpValue[m++];
  zvsdfactor = tmpValue[m++];
  guidewiresep = tmpValue[m++];
  angleOfUV1Slots = tmpValue[m++] / ToDegree;       // rad, fixed angle of UV1 slots
  angleOfUV2Slots = tmpValue[m++] / ToDegree;       // rad, fixed angle of UV2 slots

  dcwestxo = tmpValue[m++];
  dcwestyo = tmpValue[m++];
  dcwestzo = tmpValue[m++];
  dceastxo = tmpValue[m++];
  dceastyo = tmpValue[m++];
  dceastzo = tmpValue[m++];

  dcwestxa = tmpValue[m++];
  dcwestya = tmpValue[m++];
  dcwestza = tmpValue[m++];
  dceastxa = tmpValue[m++];
  dceastya = tmpValue[m++];
  dceastza = tmpValue[m++];

  for (int plane = 0; plane < numberOfPlanes;plane++)
    {
      radiusAtExtremZ[plane] = tmpValue[m++];
      lengthOfXWire[plane] = suppzlength;
    }

  bottomPhiOfArm[WEST] = phibotw / ToDegree ;
  topPhiOfArm[WEST] = phitopw / ToDegree ;
  bottomPhiOfArm[EAST] = phibote / ToDegree ;
  topPhiOfArm[EAST] = phitope / ToDegree ;
  deltaRadiusOfCell = planethick ;
  deltaPhiOfCage = uvangle / ToDegree;
  deltaPhiOfCell = deltaPhiOfCage / 4.;
  distanceToDriftRegion = propregwidth;

  return True;
}

PHBoolean
PHDchGeometryObject::fetchWirePositions()
{
  int arm, plane, cell;
  int length = geometryBank->getLength();

  PdbDchWire *wire;

  for (int i = 0; i < length; i++)
    {
      wire = (PdbDchWire*) & geometryBank->getEntry(i);
      dchaddress->getGlobalIndex()->setValue(i);                  // set the value of the global index
      dchaddress->setGlobalIndex(dchaddress->getGlobalIndex());      // update all the indices according to the global index
      arm = dchaddress->getArm()->getValue();
      plane = dchaddress->getPlane()->getValue();
      cell = dchaddress->getCell()->getValue();

      wireBasepointNorth[arm][plane][cell] = wire->getNorthPoint();
      wireBasepointSouth[arm][plane][cell] = wire->getSouthPoint();
      wireDriftDirectionNorth[arm][plane][cell] = wire->getNorthDrift();
      wireDriftDirectionSouth[arm][plane][cell] = wire->getSouthDrift();

    }

  return True;
}

PHBoolean
PHDchGeometryObject::fetchAlphas()
{
  //  OK gang...now it gets intense.
  //  In this routine we will be _retreiving_ the entire bank
  //  of alpha calibration parameters...YEE HAW!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  \  entries == number of entries that *SHOULD* follow
  //
  //      /   set   (X1 or X2)
  //  b  |    arm
  //  o  |    side
  //  d  |    cell
  //  y   \   alpha
  //
  //  The number of times the body repeats will be 2*2*2*80
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 1-21-2004
  //

  int length = 5*numberOfWireTypes*numberOfArms*numberOfSides*numberOfCells + 2; // array + 2 hdr
  int truelength = alphaBank->getLength();
  if (length != truelength)
    {
      cout << "PHDchGeometryObject:: FATAL...wrong length DB read for alpha" << endl;
      return False;
    }


  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
  int scheme = (int)parameter->getParameter();
  if (scheme != 1)
    {
      cout << "PHDchGeometryObject:: FATAL...wrong scheme DB read for alpha" << endl;
      return False;
    }


  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
  int entries = (int)parameter->getParameter();
  if (entries != length-2)
    {
      cout << "PHDchGeometryObject:: FATAL...wrong entries DB read for alpha" << endl;
      return False;
    }


  for (int i=0; i<numberOfWireTypes; i++) {
    for (int j=0; j<numberOfArms; j++) {
      for (int k=0; k<numberOfSides; k++) {
	for (int l=0; l<numberOfCells; l++) {
	  
	  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
	  int wire = (int)parameter->getParameter();

	  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
	  int arm = (int)parameter->getParameter();

	  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
	  int side = (int)parameter->getParameter();

	  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
	  int cell = (int)parameter->getParameter();

	  parameter = (PdbParameter *)& alphaBank->getEntry(index++);
	  A0[wire][arm][side][cell] = parameter->getParameter();

	}
      }
    }
  }

  //  OK, now apply the alphas...
  nudgeCardAngles();

  return True;
}

PHBoolean
PHDchGeometryObject::fetchTilts()
{
  //  OK gang...now it gets intense.
  //  In this routine we will be _retreiving_ the entire bank
  //  of tilt calibration parameters...YEE HAW!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  \  entries == number of entries that *SHOULD* follow
  //
  //      /   arm
  //  b  |    cell
  //  o  |    x1Slope
  //  d  |    x1Offset
  //  y  |    x2Slope
  //      \   x2Offset
  //
  //  The number of times the body repeats will be 2*80
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 1-21-2004
  //

  int length = 6*numberOfArms*numberOfCells + 2; // array + 2 hdr
  int truelength = tiltBank->getLength();
  if (length != truelength)
    {
      cout << "PHDchGeometryObject:: FATAL...wrong length DB read for tilt" << endl;
      return False;
    }

  PdbParameter *parameter;
  int index = 0;

  parameter = (PdbParameter *)& tiltBank->getEntry(index++);
  int scheme = (int)parameter->getParameter();
  if (scheme != 1)
    {
      cout << "PHDchGeometryObject:: FATAL...wrong scheme DB read for tilt" << endl;
      return False;
    }


  parameter = (PdbParameter *)& tiltBank->getEntry(index++);
  int entries = (int)parameter->getParameter();
  if (entries != length-2)
    {
      cout << "PHDchGeometryObject:: FATAL...wrong entries DB read for tilt" << endl;
      return False;
    }



  for (int i=0; i<numberOfArms; i++) {
    for (int j=0; j<numberOfCells; j++) {
      
      parameter = (PdbParameter *)& tiltBank->getEntry(index++);
      int arm = (int)parameter->getParameter();
      
      parameter = (PdbParameter *)& tiltBank->getEntry(index++);
      int cell = (int)parameter->getParameter();
      
      parameter = (PdbParameter *)& tiltBank->getEntry(index++);
      x1Slope[arm][cell] = parameter->getParameter();
      
      parameter = (PdbParameter *)& tiltBank->getEntry(index++);
      x1Offset[arm][cell] = parameter->getParameter();
      
      parameter = (PdbParameter *)& tiltBank->getEntry(index++);
      x2Slope[arm][cell] = parameter->getParameter();
      
      parameter = (PdbParameter *)& tiltBank->getEntry(index++);
      x2Offset[arm][cell] = parameter->getParameter();
      
    }
  }
  
  return True;
}

PHBoolean
PHDchGeometryObject::fetchBeams()
{
  //  OK gang...now it gets intense.
  //  In this routine we will be _retreiving_ the entire bank
  //  of beam calibration parameters...YEE HAW!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  \  entries == number of entries that *SHOULD* follow
  //
  //  b  /  xBeamOffset 
  //  d  \  yBeamOffset
  //
  //  The number of times the body repeats will be 1
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 1-21-2004
  //
  //  This is now updated to understand BOTH scheme 1 and scheme 2.
  //  If the data in the database is scheme1, then the pos values are
  //  written into BOTH the east and west.  If the scheme is 2,
  //  then independent values are read for the two arms...
  //
  //                         TKH RP 3-16-2009
  //
  //
  //

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *)& beamBank->getEntry(index++);
  int scheme = (int)parameter->getParameter();
  if (scheme == 1)
    {
      int length = 4; // array + 2 hdr
      int truelength = beamBank->getLength();
      if (length != truelength)
	{
	  cout << "PHDchGeometryObject:: FATAL...wrong length DB read for beam" << endl;
	  return False;
	}
      
      parameter = (PdbParameter *)& beamBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != 2)
	{
	  cout << "PHDchGeometryObject:: FATAL...wrong scheme DB read for beam" << endl;
	  return False;
	}
      
      parameter = (PdbParameter *)& beamBank->getEntry(index++);
      float xBeamOffset = parameter->getParameter();
      xBeamOffsetEast = xBeamOffset;
      xBeamOffsetWest = xBeamOffset;
      
      parameter = (PdbParameter *)& beamBank->getEntry(index++);
      float yBeamOffset = parameter->getParameter();
      yBeamOffsetEast = yBeamOffset;
      yBeamOffsetWest = yBeamOffset;
      
      return True;
    }

  if (scheme == 2)
    {
      int length = 6; // array + 2 hdr
      int truelength = beamBank->getLength();
      if (length != truelength)
	{
	  cout << "PHDchGeometryObject:: FATAL...wrong length DB read for beam" << endl;
	  return False;
	}
      
      parameter = (PdbParameter *)& beamBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != 4)
	{
	  cout << "PHDchGeometryObject:: FATAL...wrong scheme DB read for beam" << endl;
	  return False;
	}
      
      parameter = (PdbParameter *)& beamBank->getEntry(index++);
      xBeamOffsetEast = parameter->getParameter();
      
      parameter = (PdbParameter *)& beamBank->getEntry(index++);
      yBeamOffsetEast = parameter->getParameter();
      
      parameter = (PdbParameter *)& beamBank->getEntry(index++);
      xBeamOffsetWest = parameter->getParameter();
      
      parameter = (PdbParameter *)& beamBank->getEntry(index++);
      yBeamOffsetWest = parameter->getParameter();
      
      return True;
    }


  cout << "PHDchGeometryObject:: FATAL...wrong scheme DB read for beam" << endl;
  return False;

}

PHBoolean
PHDchGeometryObject::fetchUvtts()
{
  //  OK gang...now it gets intense.
  //  In this routine we will be _retreiving_ the entire bank
  //  of uvtt calibration parameters...YEE HAW!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  \  entries == number of entries that *SHOULD* follow
  //
  //  b   /   arm
  //  o  |    wire
  //  d  |    uvSlope
  //  y   \   uvOffset
  //
  //  The number of times the body repeats will be 2*16
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 1-21-2004
  //

  int length = 4*numberOfArms*16 + 2; // array + 2 hdr
  int truelength = uvttBank->getLength();
  if (length != truelength)
    {
      cout << "PHDchGeometryObject:: FATAL...wrong length DB read for uvtt" << endl;
      return False;
    }

  PdbParameter *parameter;
  int index = 0;

  parameter = (PdbParameter *)& uvttBank->getEntry(index++);
  int scheme = (int)parameter->getParameter();
  if (scheme != 1)
    {
      cout << "PHDchGeometryObject:: FATAL...wrong scheme DB read for uvtt" << endl;
      return False;
    }


  parameter = (PdbParameter *)& uvttBank->getEntry(index++);
  int entries = (int)parameter->getParameter();
  if (entries != length-2)
    {
      cout << "PHDchGeometryObject:: FATAL...wrong entries DB read for uvtt" << endl;
      return False;
    }



  for (int i=0; i<numberOfArms; i++) {
    for (int j=0; j<16; j++) {
      
      parameter = (PdbParameter *)& uvttBank->getEntry(index++);
      int arm = (int)parameter->getParameter();
      
      parameter = (PdbParameter *)& uvttBank->getEntry(index++);
      int wire = (int)parameter->getParameter();
      
      parameter = (PdbParameter *)& uvttBank->getEntry(index++);
      uvSlope[arm][wire] = parameter->getParameter();
      
      parameter = (PdbParameter *)& uvttBank->getEntry(index++);
      uvOffset[arm][wire] = parameter->getParameter();
            
    }
  }
  
  //  OK...now APPLY the tilts for x AND uv...
  nudgeWireLocations();

  return True;
}

DchWireType
PHDchGeometryObject::returnWireType(short plane)
{
  DchWireType string;
  
  if (plane < numberOfXPlanes)
    {
      string = X1;
    }
  else if (plane < numberOfXPlanes + numberOfUorVPlanes)
    {
      string = U1;
    }
  else if (plane < halfNumberOfPlanes)
    {
      string = V1;
    }
  else if (plane < halfNumberOfPlanes + numberOfXPlanes)
    {
      string = X2;
    }
  else if (plane < halfNumberOfPlanes + numberOfXPlanes + numberOfUorVPlanes)
    {
      string = U2;
    }
  else if (plane < numberOfPlanes)
    {
      string = V2;
    }
  else
    {
      cout << __FILE__ << ":" << __LINE__
	   << ": Illegal DCH plane number! Setting DchWireType to X1!" << endl;
      string = X1;
    }

  return string;
}

void
PHDchGeometryObject::calculateAverageRadius()
{
  if (fabs(radiusAtExtremZ[0])<100)
    {
      cout << PHWHERE << " radius at extreme Z has not been initialized " << endl;
      cout << " exiting " << endl;
      exit(1);
    }

  averageRadius[0] = radiusAtExtremZ[0];
  averageRadius[1] = radiusAtExtremZ[1];
  averageRadius[2] = radiusAtExtremZ[2];
  averageRadius[3] = radiusAtExtremZ[3];
  averageRadius[4] = radiusAtExtremZ[4];
  averageRadius[5] = radiusAtExtremZ[5];
  averageRadius[6] = radiusAtExtremZ[6];
  averageRadius[7] = radiusAtExtremZ[7];
  averageRadius[8] = radiusAtExtremZ[8];
  averageRadius[9] = radiusAtExtremZ[9];
  averageRadius[10] = radiusAtExtremZ[10];
  averageRadius[11] = radiusAtExtremZ[11];
  averageRadius[12] = radiusAtExtremZ[12] * 0.99942;
  averageRadius[13] = radiusAtExtremZ[13] * 0.99942;
  averageRadius[14] = radiusAtExtremZ[14] * 0.99942;
  averageRadius[15] = radiusAtExtremZ[15] * 0.99942;
  averageRadius[16] = radiusAtExtremZ[16] * 0.99942;
  averageRadius[17] = radiusAtExtremZ[17] * 0.99942;
  averageRadius[18] = radiusAtExtremZ[18] * 0.99942;
  averageRadius[19] = radiusAtExtremZ[19] * 0.99942;
  averageRadius[20] = radiusAtExtremZ[20];
  averageRadius[21] = radiusAtExtremZ[21];
  averageRadius[22] = radiusAtExtremZ[22];
  averageRadius[23] = radiusAtExtremZ[23];
  averageRadius[24] = radiusAtExtremZ[24];
  averageRadius[25] = radiusAtExtremZ[25];
  averageRadius[26] = radiusAtExtremZ[26];
  averageRadius[27] = radiusAtExtremZ[27];
  averageRadius[28] = radiusAtExtremZ[28];
  averageRadius[29] = radiusAtExtremZ[29];
  averageRadius[30] = radiusAtExtremZ[30];
  averageRadius[31] = radiusAtExtremZ[31];
  averageRadius[32] = radiusAtExtremZ[32] * 0.99942;
  averageRadius[33] = radiusAtExtremZ[33] * 0.99942;
  averageRadius[34] = radiusAtExtremZ[34] * 0.99942;
  averageRadius[35] = radiusAtExtremZ[35] * 0.99942;
  averageRadius[36] = radiusAtExtremZ[36] * 0.99942;
  averageRadius[37] = radiusAtExtremZ[37] * 0.99942;
  averageRadius[38] = radiusAtExtremZ[38] * 0.99942;
  averageRadius[39] = radiusAtExtremZ[39] * 0.99942;
}

void
PHDchGeometryObject::initializeFlagXUV()
{
  for (int plane = 0; plane < numberOfPlanes; plane++)
    {
      if (plane < numberOfXPlanes)
	{
	  flagXUV[plane] = 0;
	}
      else if (plane < (numberOfXPlanes + numberOfUorVPlanes))
	{
	  flagXUV[plane] = 1;
	}
      else if (plane < (halfNumberOfPlanes))
	{
	  flagXUV[plane] = -1;
	}
      else if (plane < (halfNumberOfPlanes + numberOfXPlanes))
	{
	  flagXUV[plane] = 0;
	}
      else if (plane < (halfNumberOfPlanes + numberOfXPlanes + numberOfUorVPlanes))
	{
	  flagXUV[plane] = 1;
	}
      else if (plane < numberOfPlanes)
	{
	  flagXUV[plane] = -1;
	}
    }
}

PHLine
PHDchGeometryObject::transformDistanceToLine(const short& arm,
					     const short& plane,
					     const short& cell,
					     const float& distance)
{
  //  This function returns a line parallel to the wire , the basepoint is translated
  //  in the drift direction of 'distance'
  //
  //  note distances MUST have a sign provided !!!
  //  you do not need any more the step from cell to local coordinate;
  //

  PHLine line;                                                                    // return value in DC coordinates
  PHPoint pointNorth = wireBasepointNorth[arm][plane][cell];       // basepoint  in local coordinates from DB
  PHVector driftDirectionNorth = wireDriftDirectionNorth[arm][plane][cell];  // drift direction in local coord. from DB
  PHPoint pointSouth = wireBasepointSouth[arm][plane][cell];       // basepoint  in local coordinates from DB
  PHVector driftDirectionSouth = wireDriftDirectionSouth[arm][plane][cell];  // drift direction in local coord. from DB

  PHPoint transPointNorth = pointNorth + (PHPoint)driftDirectionNorth * (distance);
  PHPoint transPointSouth = pointSouth + (PHPoint)driftDirectionSouth * (distance);

  line.setDirection(transPointSouth - transPointNorth);
  line.setBasepoint(transPointNorth);

  return line;

} 
  
PHLine PHDchGeometryObject::transformDistanceToLine(const short& arm,
						    const short& side,
						    const short& plane,
						    const short& cell,
						    const PHVector& dist)
{
  //  This function returns a line parallel to the wire , the basepoint is translated
  //  in the drift direction of 'distance'    
  //
  //  note distances MUST have a sign provided !!!
  //  you do not need any more the step from cell to local coordinate;
  //
  //  This version also shifts the radius of the point
  //
  //
    
  static PHPoint  zedNorth(0.,0., 50.);
  static PHPoint  zedSouth(0.,0.,-50.);
  static PHVector zedAxis(0.,0.,1.);
  static PHPlane northPlane(zedNorth, zedAxis);
  static PHPlane southPlane(zedSouth, zedAxis);

  PHLine line;                                                                    // return value in DC coordinates
  PHPoint pointNorth = wireBasepointNorth[arm][plane][cell];       // basepoint  in local coordinates from DB
  PHVector driftDirectionNorth = wireDriftDirectionNorth[arm][plane][cell];  // drift direction in local coord. from DB
  PHPoint pointSouth = wireBasepointSouth[arm][plane][cell];       // basepoint  in local coordinates from DB
  PHVector driftDirectionSouth = wireDriftDirectionSouth[arm][plane][cell];  // drift direction in local coord. from DB
  
  PHPoint transPointNorth     = pointNorth + (PHPoint)driftDirectionNorth*(dist.getX());
  PHPoint transPointSouth     = pointSouth + (PHPoint)driftDirectionSouth*(dist.getX());
  
  line.setDirection(transPointSouth-transPointNorth);

  PHCylPoint temp = transPointNorth;
  temp.setR(temp.getR()+dist.getY());
  transPointNorth = temp;
  line.setBasepoint(transPointNorth);

  // Move the basepoint to either +/- 50 cm in Z:
  PHPoint intersection(0,0,0);
  PHBoolean doesIntersect;
  if (side==(short)NORTH) {
    doesIntersect = intersectionLinePlane(line, northPlane, intersection);
  }
  else {
    doesIntersect = intersectionLinePlane(line, southPlane, intersection);
  }
  if (doesIntersect) line.setBasepoint(intersection);

  return line;
} 
  
//  If not you do not use the info in the database and you have the MC
//  parameter from a STAF table (or similar) you should use this too
//  function to calculate the Distance to Line !!!!  The function
//  "transformFromCellToLocalCoord" is called internally in the
//  "transformDistanceToLineOld" Otherwise if you load the wire
//  positions form the database then you need to call the function
//  "transformDistanceToLine" !!
PHPoint
PHDchGeometryObject::transformFromCellToLocalCoord(const short& arm,
						   const short& plane,
						   const short& cell,
						   const PHPoint& inCellCoord)
{
  PHPoint inLocalCoord;     // return value in DC coordinates
  float phi;            // locally used variables

  // Rotate drift cell to phi=0 and shift by radius of plane (at
  // center) in +x direction
  float x, y;
  float pp = -.00982;
  x = inCellCoord.getX() * cos(pp) - inCellCoord.getY() * sin(pp);
  y = inCellCoord.getY() * cos(pp) + inCellCoord.getX() * sin(pp);

  inLocalCoord.setX(inCellCoord.getY() + radiusAtZ0[plane]*.999951) ;
  inLocalCoord.setZ(inCellCoord.getZ());
  if (arm == west)
    {
      inLocalCoord.setY( -inCellCoord.getX()) ;
    }
  else if (arm == east)
    {
      inLocalCoord.setX(y + radiusAtZ0[plane]*.999951);
      inLocalCoord.setY( -x);
    }
  else
    {
      PHMessage("PHDchGeometryObject::transformFromCellToLocalCoord", PHError, "arm id is too big !!!! ");
    }

  PHCylPoint cylpoint = inLocalCoord;
  phi = cylpoint.getPhi();  // to be checked (it was : inLocalCoord.phi();)

  //
  //  add phi of cell
  //  numbering convention:
  //   west arm start at bottom in math. positive phi direction
  //   east arm start at bottom in math. negative phi direction
  //
  if (arm == west)
    {            // count from bot
      phi += bottomPhiOfArm[west] + (cell + .25) * deltaPhiOfCell;
    }
  else if (arm == east)
    {
      phi += bottomPhiOfArm[east] - (cell + .25) * deltaPhiOfCell;
    }
  else
    {
      PHMessage("PHDchGeometryObject::transformFromCellToLocalCoord", PHError, 
		"arm id is too big!");
    }

  // rotate vector to new phi and exit

  cylpoint.setPhi(phi);
  inLocalCoord = cylpoint; //to be checked (it was: inLocalCoord.setPhi(phi);)
  return inLocalCoord;
}

PHLine
PHDchGeometryObject::transformDistanceToLineOld (const short& arm,
						 const short& plane,
						 const short& cell,
						 const float& distance)
{
  //
  // note distances MUST have a sign provided !!!
  //
  PHLine line;                // return value in DC coordinates

  if (flagXUV[plane] == 0)
    {   // use geometry to do xwires

      PHPoint point(0, 0, 0);
      line.setDirection(wireLine[plane].getDirection());  // direction of wire
      point = wireInCellCoord[north][plane];           // wire end points
      point.setX(distance);                            //
      line.setBasepoint(transformFromCellToLocalCoord(arm, plane, cell, point));
    }
  else
    {                             // full calculation for U and V
      PHPoint point, point1, point2;
      float x, y, dx, dy;
      // calculate dx,dy at face
      dx = distance * cos(2 * deltaPhiOfCell);   // in cell coordinates
      dy = flagXUV[plane] * distance * sin(2 * deltaPhiOfCell);

      point = wireInCellCoord[north][plane];  // get north end of wire
      x = point.getX() + dx;                     // shift x by dx
      y = point.getY() + dy;                     // shift y by dy
      point.setX(x);
      point.setY(y);
      point1 = transformFromCellToLocalCoord(arm, plane, cell, point);

      point = wireInCellCoord[south][plane];  // same thing for south end
      x = point.getX() + dx;                     // same as north
      y = point.getY() - dy;                     // beam angle has differnt sign
      point.setX(x);
      point.setY(y);
      point2 = transformFromCellToLocalCoord(arm, plane, cell, point);

      line.setBasepoint(point1);                     // set basepoint
      line.setDirection(point2 - point1);              // and direction
    }

  return line;
}

PHLine
PHDchGeometryObject::transformDistanceToLineOld2 (const short& arm,
						  const short& plane,
						  const short& cell,
						  const float& distance)
{
  //
  // note distances MUST have a sign provided !!!
  //
  PHLine line;                // return value in DC coordinates

  if (flagXUV[plane] == 0)
    {   // use geometry to do xwires

      PHPoint point(0, 0, 0);
      line.setDirection(wireLine[plane].getDirection());  // direction of wire
      point = wireInCellCoord[north][plane];           // wire end points
      point.setX(distance);                            //
      line.setBasepoint(transformFromCellToLocalCoord(arm, plane, cell, point));
      line.setBasepoint(line.getBasepoint() + (PHPoint)line.getDirection());
    }
  else
    {                             // full calculation for U and V

      PHPoint point, point1, point2;
      float x, y, dx, dy;
      // calculate dx,dy at face
      dx = distance * cos(2 * deltaPhiOfCell);   // in cell coordinates
      dy = flagXUV[plane] * distance * sin(2 * deltaPhiOfCell);

      point = wireInCellCoord[north][plane];  // get north end of wire
      x = point.getX() + dx;                     // shift x by dx
      y = point.getY() + dy;                     // shift y by dy
      point.setX(x);
      point.setY(y);
      point1 = transformFromCellToLocalCoord(arm, plane, cell, point);

      point = wireInCellCoord[south][plane];  // same thing for south end
      x = point.getX() + dx;                     // same as north
      y = point.getY() - dy;                     // tilt angle has differnt sign
      point.setX(x);
      point.setY(y);
      point2 = transformFromCellToLocalCoord(arm, plane, cell, point);

      line.setBasepoint(point2);                     // set basepoint
      line.setDirection(point2 - point1);              // and direction
    }

  return line;
}

short
PHDchGeometryObject::getWireSide(PHPoint point, short a, short p, short c)
{
  PHCylPoint cylPoint = point;

  PHAngle phiPoint = cylPoint.getPhi();
  PHAngle phiWire = cylPoint.getPhi();

  if (phiPoint > phiWire)
    {
      return 1;
    }
  else
    {
      return -1;
    }
}

short
PHDchGeometryObject::initializeWireLine()
{
  PHPoint noDrift(0., 0., 0.);
  PHPoint drift(0., 0., 0.);
  PHLine thisLine0;
  PHLine thisLine1;
  double zeroValue = 0.;
  double oneValue = 1.;
  int global = 0;
  for (short arm = 0; arm < numberOfArms; arm++)
    {
      for (short plane = 0; plane < numberOfPlanes; plane++)
	{
	  for (short cell = 0; cell < numberOfCells; cell++)
	    {
	      thisLine0 = transformDistanceToLineOld(arm, plane, cell, zeroValue);
	      thisLine1 = transformDistanceToLineOld(arm, plane, cell, oneValue);
	      noDrift = thisLine0.getBasepoint();
	      drift = thisLine1.getBasepoint();
	      wireBasepointNorth[arm][plane][cell] = thisLine0.getBasepoint();
	      wireDriftDirectionNorth[arm][plane][cell] = drift - noDrift;

	      thisLine0 = transformDistanceToLineOld2(arm, plane, cell, zeroValue);
	      thisLine1 = transformDistanceToLineOld2(arm, plane, cell, oneValue);
	      noDrift = thisLine0.getBasepoint();
	      drift = thisLine1.getBasepoint();
	      wireBasepointSouth[arm][plane][cell] = thisLine0.getBasepoint();
	      wireDriftDirectionSouth[arm][plane][cell] = drift - noDrift;
	      global++;
	    }
	}
    }

  return 1;
}

PHBoolean
PHDchGeometryObject::fetchFramesFromFile()
{
  ifstream file(geoFrameFile);
  if (!file)
    {
      cout << " could not open input file " << geoFrameFile << " !!" << endl;
      return False;
    }
  double value;
  string parName;
  int numberOfValues = 0;
  int length = 12 + 12;               // 2 arms * vector * matrix

  parameterFrame.clear();
  while (!file.eof() && numberOfValues < length)
    {
      file >> value >> parName;
      tmpValueFrame[numberOfValues] = value;
      parameterFrame.push_back(parName);
      numberOfValues++;
    }

  int m = 0;
  double oxe  = tmpValueFrame[m++];  // origin X
  double oye  = tmpValueFrame[m++];
  double oze  = tmpValueFrame[m++];
  double a1xe = tmpValueFrame[m++]; // first axis of the e detector : x coordinate
  double a1ye = tmpValueFrame[m++];
  double a1ze = tmpValueFrame[m++];
  double a2xe = tmpValueFrame[m++];
  double a2ye = tmpValueFrame[m++];
  double a2ze = tmpValueFrame[m++];
  double a3xe = tmpValueFrame[m++];
  double a3ye = tmpValueFrame[m++];
  double a3ze = tmpValueFrame[m++];

  double oxw  = tmpValueFrame[m++];
  double oyw  = tmpValueFrame[m++];
  double ozw  = tmpValueFrame[m++];
  double a1xw = tmpValueFrame[m++];
  double a1yw = tmpValueFrame[m++];
  double a1zw = tmpValueFrame[m++];
  double a2xw = tmpValueFrame[m++];
  double a2yw = tmpValueFrame[m++];
  double a2zw = tmpValueFrame[m++];
  double a3xw = tmpValueFrame[m++];
  double a3yw = tmpValueFrame[m++];
  double a3zw = tmpValueFrame[m++];

  PHPoint originE(oxe, oye, oze);
  PHPoint originW(oxw, oyw, ozw);

  PHVector uE(a1xe, a1ye, a1ze);
  PHVector vE(a2xe, a2ye, a2ze);
  PHVector wE(a3xe, a3ye, a3ze);

  PHVector uW(a1xw, a1yw, a1zw);
  PHVector vW(a2xw, a2yw, a2zw);
  PHVector wW(a3xw, a3yw, a3zw);

  uE.normalize();
  vE.normalize();
  wE.normalize();

  uW.normalize();
  vW.normalize();
  wW.normalize();

  PHVector v1;
  PHVector v2;
  v1 = uE.cross(vE);
  v1.normalize();
  v2 = v1.cross(uE);
  v2.normalize();

  vE = v2;
  wE = uE.cross(vE);

  v1 = uW.cross(vW);
  v1.normalize();
  v2 = v1.cross(uW);
  v2.normalize();

  vW = v2;
  wW = uW.cross(vW);

  PHFrame initialE;
  PHFrame initialW;

  PHFrame finalE(originE, uE, vE);
  PHFrame finalW(originW, uW, vW);

  startingFrame[EAST] = initialE;
  startingFrame[WEST] = initialW;
  endingFrame[EAST] = finalE;
  endingFrame[WEST] = finalW;

  return True;
}

PHBoolean
PHDchGeometryObject::fetchGeometryInfoFromFile()
{
  ifstream file(geoInfoFile);
  if (!file)
    {
      cout << " could not open input file " << geoInfoFile << " !!" << endl;
      return False;
    }
  float value;
  string parName;
  int numberOfValues = 0;
  // total number of  values from old Staf table + 2 + 12(cylinder sections)
  int length = 97 + 12;      

  parameterName.clear();
  while (!file.eof() && numberOfValues < length)
    {
      file >> value >> parName;
      tmpValue[numberOfValues] = value;
      parameterName.push_back(parName);
      numberOfValues++;
    }

  int m = 0;
  ncells = tmpValue[m++];
  ngusset = tmpValue[m++];
  ti_switch = tmpValue[m++];
  suppzlength = tmpValue[m++];
  innerRadius = tmpValue[m++];
  outerRadius = tmpValue[m++];
  phibotw = tmpValue[m++];
  phitopw = tmpValue[m++];
  phitope = tmpValue[m++];
  phibote = tmpValue[m++];
  planethick = tmpValue[m++];
  uvangle = tmpValue[m++];
  winthickin = tmpValue[m++];
  winthickout = tmpValue[m++];
  supptiside = tmpValue[m++];
  suppalside = tmpValue[m++];
  suppzthick = tmpValue[m++];
  supptibase = tmpValue[m++];
  suppalbase = tmpValue[m++];
  x1baserad = tmpValue[m++];
  x2baserad = tmpValue[m++];
  x1basez = tmpValue[m++];
  x2basez = tmpValue[m++];
  x1slotthick = tmpValue[m++];
  x2slotthick = tmpValue[m++];
  x1slotz = tmpValue[m++];
  x2slotz = tmpValue[m++];
  x1suppthick = tmpValue[m++];
  x2suppthick = tmpValue[m++];
  x1suppz = tmpValue[m++];
  x2suppz = tmpValue[m++];
  x1rextent = tmpValue[m++];
  x2rextent = tmpValue[m++];
  u1rextent = tmpValue[m++];
  v1rextent = tmpValue[m++];
  u2rextent = tmpValue[m++];
  v2rextent = tmpValue[m++];
  u1basez = tmpValue[m++];
  v1basez = tmpValue[m++];
  u2basez = tmpValue[m++];
  v2basez = tmpValue[m++];
  u1slotz = tmpValue[m++];
  v1slotz = tmpValue[m++];
  u2slotz = tmpValue[m++];
  v2slotz = tmpValue[m++];
  u1suppz = tmpValue[m++];
  v1suppz = tmpValue[m++];
  u2suppz = tmpValue[m++];
  v2suppz = tmpValue[m++];
  cfibinrad = tmpValue[m++];
  cfiboutrad = tmpValue[m++];
  propregwidth = tmpValue[m++];
  zvsdfactor = tmpValue[m++];
  guidewiresep = tmpValue[m++];
  angleOfUV1Slots = tmpValue[m++] / ToDegree;       // rad, fixed angle of UV1 slots
  angleOfUV2Slots = tmpValue[m++] / ToDegree;       // rad, fixed angle of UV2 slots

  dcwestxo = tmpValue[m++];  // x origin of West
  dcwestyo = tmpValue[m++];
  dcwestzo = tmpValue[m++];
  dceastxo = tmpValue[m++];
  dceastyo = tmpValue[m++];
  dceastzo = tmpValue[m++];

  dcwestxa = tmpValue[m++]; // x axis of West
  dcwestya = tmpValue[m++];
  dcwestza = tmpValue[m++];
  dceastxa = tmpValue[m++];
  dceastya = tmpValue[m++];
  dceastza = tmpValue[m++];

  for (int plane = 0; plane < numberOfPlanes;plane++)
    {
      radiusAtExtremZ[plane] = tmpValue[m++];
      lengthOfXWire[plane] = suppzlength;
    }

  bottomPhiOfArm[WEST] = phibotw / ToDegree ;
  topPhiOfArm[WEST] = phitopw / ToDegree ;
  bottomPhiOfArm[EAST] = phibote / ToDegree ;
  topPhiOfArm[EAST] = phitope / ToDegree ;
  deltaRadiusOfCell = planethick ;
  deltaPhiOfCage = uvangle / ToDegree;
  deltaPhiOfCell = deltaPhiOfCage / 4.;
  distanceToDriftRegion = propregwidth;

  return True;
}

//________________________________________________________________________
PHBoolean PHDchGeometryObject::fetchWirePositionsFromFile()
{
  // check that file was set
  if( !geoWireFile )
  {
    cout << "PHDchGeometryObject::fetchWirePositionsFromFile - wire geometry file not set" << endl;
    return False;       
  }
  
  // check that file exists
  ifstream file(geoWireFile);
  if (!file)
  {
    cout << "PHDchGeometryObject::fetchWirePositionsFromFile - could not open input file " << geoWireFile << endl;
    return False;
  }
  
  // the entry are --> basepoint, wire direction, drift direction
  int global, arm, plane, cell;
  float bnx, bny, bnz, bsx, bsy, bsz, ddnx, ddny, ddnz, ddsx, ddsy, ddsz;
  int numberOfWires = 0;
  
  int oldGlobal = 0;
  
  while (!file.eof())
  {
    file >> global >> arm >> plane >> cell
      >> bnx >> bny >> bnz >> bsx >> bsy >> bsz
      >> ddnx >> ddny >> ddnz >> ddsx >> ddsy >> ddsz;
    if (global > oldGlobal)
      oldGlobal = global;
    
    if (numberOfWires < dchaddress->getGlobalIndex()->getNumberOf() / 2.0)
    {
      wireBasepointNorth[arm][plane][cell].setX(bnx);
      wireBasepointNorth[arm][plane][cell].setY(bny);
      wireBasepointNorth[arm][plane][cell].setZ(bnz);
      wireBasepointSouth[arm][plane][cell].setX(bsx);
      wireBasepointSouth[arm][plane][cell].setY(bsy);
      wireBasepointSouth[arm][plane][cell].setZ(bsz);
      wireDriftDirectionNorth[arm][plane][cell].setX(ddnx);
      wireDriftDirectionNorth[arm][plane][cell].setY(ddny);
      wireDriftDirectionNorth[arm][plane][cell].setZ(ddnz);
      wireDriftDirectionSouth[arm][plane][cell].setX(ddsx);
      wireDriftDirectionSouth[arm][plane][cell].setY(ddsy);
      wireDriftDirectionSouth[arm][plane][cell].setZ(ddsz);
    } else break;
    
    if (file.eof())
    {
      cout << "PHDchGeometryObject::fetchWirePositionsFromFile - End of file reached" << endl;
      break;
    }
  }
    
  return True;
}

PHBoolean
PHDchGeometryObject::fetchAlphasFromFile()
{
  // Don't try to read if filename is NONE...
  if (strcmp(geoAlphaFile,"NONE")==0) return True;

  ifstream file(geoAlphaFile);
  if (!file)
    {
      cout << "PHDchGeometryObject:: Could not open input file " << geoAlphaFile << " !!" << endl;
      return False;
    }

  //  Each line of the text file contains the following:
  //
  //      /   set   (X1 or X2)
  //  b  |    arm
  //  o  |    side
  //  d  |    cell
  //  y   \   alpha
  //
  //  The number of lines will be 2*2*2*80
  //
  //                          TKH 1-22-2004
  //

  int wire, arm, side, cell;
  float alpha;
  
  if (verbose>0) cout << "Set Arm Side Cell Alpha" << endl;
  while (!file.eof())
    {
      file >> wire >> arm >> side >> cell >> alpha;
      A0[wire][arm][side][cell] = alpha;
      if (verbose>0)
	{
	  cout << wire  << " ";
	  cout << arm   << " ";
	  cout << side  << " ";
	  cout << cell  << " ";
	  cout << alpha << endl;
	}
    }


  return True;
}

PHBoolean
PHDchGeometryObject::fetchTiltsFromFile()
{
  // Don't try to read if filename is NONE...
  if (strcmp(geoTiltFile,"NONE")==0) return True;

  ifstream file(geoTiltFile);
  if (!file)
    {
      cout << "PHDchGeometryObject:: Could not open input file " << geoTiltFile << " !!" << endl;
      return False;
    }

  //  Each line of the text file contains the following:
  //
  //      /   arm
  //  b  |    cell
  //  o  |    x1Slope
  //  d  |    x1Offset
  //  y  |    x2Slope
  //      \   x2Offset
  //
  //  The number of lines will be 2*80
  //
  //                          TKH 1-22-2004
  //

  int arm, cell;
  float x1s, x1o, x2s, x2o;
  
  if (verbose>0) cout << "Arm Cell x1Slope x1Offset x2Slope x2Offset" << endl;
  while (!file.eof())
    {
      file >> arm >> cell >> x1s >> x1o >> x2s >> x2o;
      x1Slope [arm][cell] = x1s;
      x1Offset[arm][cell] = x1o;
      x2Slope [arm][cell] = x2s;
      x2Offset[arm][cell] = x2o;
      if (verbose>0)
	{
	  cout << arm   << " ";
	  cout << cell  << " ";
	  cout << x1Slope [arm][cell]  << " ";
	  cout << x1Offset[arm][cell]  << " ";
	  cout << x2Slope [arm][cell]  << " ";
	  cout << x2Offset[arm][cell]  << endl;
	}
    }


  return True;
}

PHBoolean
PHDchGeometryObject::fetchBeamsFromFile()
{
  // Don't try to read if filename is NONE...
  if (strcmp(geoBeamFile,"NONE")==0) return True;

  ifstream file(geoBeamFile);
  if (!file)
    {
      cout << "PHDchGeometryObject:: Could not open input file " << geoBeamFile << " !!" << endl;
      return False;
    }

  //  Each line of the text file contains the following:
  //
  //  b  /    xBeamOffset
  //  d  \    yBeamOffset
  //
  //  The number of lines will be 1
  //
  //                          TKH 1-22-2004
  //

  file >> xBeamOffsetEast >> yBeamOffsetEast >> xBeamOffsetWest >> yBeamOffsetWest ;
  if (verbose>0) 
    {
      cout << "  xOffsetEast" << xBeamOffsetEast;
      cout << "  yOffsetEast" << yBeamOffsetEast;
      cout << "  xOffsetWest" << xBeamOffsetWest;
      cout << "  yOffsetWest" << yBeamOffsetWest;
      cout << endl;
    }

  return True;
}

PHBoolean
PHDchGeometryObject::fetchUvttsFromFile()
{
  // Don't try to read if filename is NONE...
  if (strcmp(geoUvttFile,"NONE")==0) return True;

  ifstream file(geoUvttFile);
  if (!file)
    {
      cout << "PHDchGeometryObject:: Could not open input file " << geoUvttFile << " !!" << endl;
      return False;
    }

  //  Each line of the text file contains the following:
  //
  //  b   /   arm
  //  o  |    wire
  //  d  |    uvSlope
  //  y   \    uvOffset
  //
  //  The number of lines will be 2*16
  //
  //                          TKH 1-22-2004
  //

  int arm, wire;
  float uvs, uvo;
  
  if (verbose>0) cout << "Arm Wire uvSlope uvOffset" << endl;
  while (!file.eof())
    {
      file >> arm >> wire >> uvs >> uvo;
      uvSlope [arm][wire] = uvs;
      uvOffset[arm][wire] = uvo;
      if (verbose>0)
	{
	  cout << arm   << " ";
	  cout << wire  << " ";
	  cout << uvSlope [arm][wire]  << " ";
	  cout << uvOffset[arm][wire]  << " ";
	  cout << endl;
	}
    }


  return True;
}

void 
PHDchGeometryObject::nudgeCardAngles()
{

  cout << " Nudging Card Angles " << endl;
  
  recoConsts *rc = recoConsts::instance();
  double alphascale =rc->get_FloatFlag("ALPHATESTFACTOR",-1.0);
  
  cout << " Overall Alpha Scale = " << alphascale << endl; 
  if (fabs(alphascale)<0.01)
    {
      cout << " No Nudge Return " << endl;
      return;
    }


 double botxN,botyN,botzN,topxN,topyN,topzN,midxN,midyN,midzN;
 double botxS,botyS,botzS,topxS,topyS,topzS,midxS,midyS,midzS;
 double alphaN,alphaS;
 double Nx,Ny,Nz,Sx,Sy,Sz;
 PHCylPoint LocalPointN,LocalPointS;
 PHPoint cLocalPointN,cLocalPointS;
 for (int iarm = 0; iarm < 2; iarm++)
   {
     for (int icell =0; icell < 80; icell++ )
       {
	 for (int iset = 0; iset <2; iset++) //X1 X2
	   {
	     // Card low R end point
	     botxN=wireBasepointNorth[iarm][0+20*iset][icell].getX();
	     botyN=wireBasepointNorth[iarm][0+20*iset][icell].getY();
	     botzN=wireBasepointNorth[iarm][0+20*iset][icell].getZ();
	     
	     // Card high R end point
	     topxN=wireBasepointNorth[iarm][11+20*iset][icell].getX();
	     topyN=wireBasepointNorth[iarm][11+20*iset][icell].getY();
	     topzN=wireBasepointNorth[iarm][11+20*iset][icell].getZ();
	     
	     // mid point of card
	     midxN=(botxN+topxN)/2.0;
	     midyN=(botyN+topyN)/2.0;
	     midzN=(botzN+topzN)/2.0;

	     alphaN=(A0[iset][iarm][1][icell]/1000.0);
	     alphaS=(A0[iset][iarm][0][icell]/1000.0);
	     	 	   	     
	     botxS=wireBasepointSouth[iarm][0+20*iset][icell].getX();
	     botyS=wireBasepointSouth[iarm][0+20*iset][icell].getY();
	     botzS=wireBasepointSouth[iarm][0+20*iset][icell].getZ();
	      
	     topxS=wireBasepointSouth[iarm][11+20*iset][icell].getX();
	     topyS=wireBasepointSouth[iarm][11+20*iset][icell].getY();
	     topzS=wireBasepointSouth[iarm][11+20*iset][icell].getZ();
	      
	     midxS=(botxS+topxS)/2.0;
	     midyS=(botyS+topyS)/2.0;
	     midzS=(botzS+topzS)/2.0;
	     
	     for (int iplane = 0; iplane < 12; iplane++)
	       {
		 Nx=wireBasepointNorth[iarm][iplane+20*iset][icell].getX();
		 Ny=wireBasepointNorth[iarm][iplane+20*iset][icell].getY();
		 Nz=wireBasepointNorth[iarm][iplane+20*iset][icell].getZ();
		 // Local Point is position with respect to card center
		 LocalPointN = (PHCylPoint) PHPoint(Nx-midxN,Ny-midyN,Nz-midzN);
		 LocalPointN.setPhi(((double) LocalPointN.getPhi())-(alphascale*alphaN));
		 cylindricalToCartesian(LocalPointN,cLocalPointN);
		 

		 Sx=wireBasepointSouth[iarm][iplane+20*iset][icell].getX();
		 Sy=wireBasepointSouth[iarm][iplane+20*iset][icell].getY();
		 Sz=wireBasepointSouth[iarm][iplane+20*iset][icell].getZ();
		 LocalPointS = (PHCylPoint) PHPoint(Sx-midxS,Sy-midyS,Sz-midzS);
		 LocalPointS.setPhi(((double) LocalPointS.getPhi())-(alphascale*alphaS));
		 cylindricalToCartesian(LocalPointS,cLocalPointS);
		 

		 // here are our new wire end points
		 wireBasepointNorth[iarm][iplane+20*iset][icell].setX(midxN+cLocalPointN.getX());
		 wireBasepointNorth[iarm][iplane+20*iset][icell].setY(midyN+cLocalPointN.getY());
		 wireBasepointSouth[iarm][iplane+20*iset][icell].setX(midxS+cLocalPointS.getX());
		 wireBasepointSouth[iarm][iplane+20*iset][icell].setY(midyS+cLocalPointS.getY());
		

	       }
	    }
       }
   }
 
 
} 

void 
PHDchGeometryObject::nudgeWireLocations() 
{
  //  Hallelujah!!  The x1 and x2 constants are in the database!!
  //  The UV constants below should follow right behind
  //  (forgot them on first cleaning pass).

  cout << " Nudging Card Tilts " << endl;
  recoConsts *rc = recoConsts::instance();
  int tiltem =rc->get_IntFlag("DCHWIRETILT",1);
  if (tiltem == 0)
    {
      cout << " No Wire Tile Nudge...Return " << endl;
      return;
    }


  PHCylPoint temp;
  float deltaPhi;

  // Nudge the X1 wires
  for (int iarm=0; iarm<2; iarm++) {
    for (int icell=0; icell<80; icell++) {
      for (int iplane=0; iplane<12; iplane++) {
	deltaPhi = x1Slope[iarm][icell]*wireBasepointNorth[iarm][iplane][icell].getZ()+x1Offset[iarm][icell];
	temp = wireBasepointNorth[iarm][iplane][icell];
	temp.setPhi((float)temp.getPhi()-deltaPhi);
	wireBasepointNorth[iarm][iplane][icell] = temp;

	deltaPhi = x1Slope[iarm][icell]*wireBasepointSouth[iarm][iplane][icell].getZ()+x1Offset[iarm][icell];
	temp = wireBasepointSouth[iarm][iplane][icell];
	temp.setPhi((float)temp.getPhi()-deltaPhi);
	wireBasepointSouth[iarm][iplane][icell] = temp;

      }
    }
  }

  // Nudge the X2 wires
  for (int iarm=0; iarm<2; iarm++) {
    for (int icell=0; icell<80; icell++) {
      for (int iplane=20; iplane<32; iplane++) {
	deltaPhi = x2Slope[iarm][icell]*wireBasepointNorth[iarm][iplane][icell].getZ()+x2Offset[iarm][icell];
	temp = wireBasepointNorth[iarm][iplane][icell];
	temp.setPhi((float)temp.getPhi()-deltaPhi);
	wireBasepointNorth[iarm][iplane][icell] = temp;

	deltaPhi = x2Slope[iarm][icell]*wireBasepointSouth[iarm][iplane][icell].getZ()+x2Offset[iarm][icell];
	temp = wireBasepointSouth[iarm][iplane][icell];
	temp.setPhi((float)temp.getPhi()-deltaPhi);
	wireBasepointSouth[iarm][iplane][icell] = temp;

      }
    }
  }

  // Nudge the UV1 wires
  for (int iarm = 0; iarm < 2; iarm++)
    {
      for (int iplane = 12; iplane < 20; iplane++)
	{
	  for (int icell =0; icell < 80; icell++ )
	    {
	      deltaPhi = uvSlope[iarm][iplane - 12] * 
		wireBasepointNorth[iarm][iplane][icell].getZ() + 
		uvOffset[iarm][iplane - 12];
	      temp = wireBasepointNorth[iarm][iplane][icell];
	      temp.setPhi((float)temp.getPhi() - deltaPhi);
	      wireBasepointNorth[iarm][iplane][icell] = temp;
	      
	      deltaPhi = uvSlope[iarm][iplane - 12] * 
		wireBasepointSouth[iarm][iplane][icell].getZ() + 
		x2Offset[iarm][iplane - 12];
	      temp = wireBasepointSouth[iarm][iplane][icell];
	      temp.setPhi((float)temp.getPhi() - deltaPhi);
	      wireBasepointSouth[iarm][iplane][icell] = temp;
	    }
	}
    }
  
  // Nudge the UV2 wires
  for (int iarm = 0; iarm < 2; iarm++)
    {
      for (int iplane = 32; iplane < 40; iplane++)
	{
	  for (int icell =0; icell < 80; icell++ )
	    {
	      deltaPhi = uvSlope[iarm][iplane - 24] *
		wireBasepointNorth[iarm][iplane][icell].getZ() +
		uvOffset[iarm][iplane - 24];
	      temp = wireBasepointNorth[iarm][iplane][icell];
	      temp.setPhi((float)temp.getPhi() - deltaPhi);
	      wireBasepointNorth[iarm][iplane][icell] = temp;
	      
	      deltaPhi = uvSlope[iarm][iplane - 24] *
		wireBasepointSouth[iarm][iplane][icell].getZ() +
		x2Offset[iarm][iplane - 24];
	      temp = wireBasepointSouth[iarm][iplane][icell];
	      temp.setPhi((float)temp.getPhi() - deltaPhi);
	      wireBasepointSouth[iarm][iplane][icell] = temp;
	    }
	}
    }
  
}



int
PHDchGeometryObject::GetBeamPosition(int run, double *xy)
{
  //  This interface is obsolete since the Beam Positions array
  //  is presently part of the PHENIX database.  However, the 
  //  interface will nonetheless be maintained by simply filling
  //  the array from the already known constants.
  //
  //                                TKH 1-22-2004
  //

  cout << " Correction for Beam Position Offset " << endl;
  recoConsts *rc = recoConsts::instance();
  int tiltem =rc->get_IntFlag("DCHBEAMPOSITION",1);
  if (tiltem == 0)
    {
      cout << " No Beam Position Correction Applied...Return " << endl;
      xy[0] = 0.0;
      xy[1] = 0.0;
      xy[2] = 0.0;
      xy[3] = 0.0;
      return -1;
    }
  
  xy[0] = xBeamOffsetEast;
  xy[1] = yBeamOffsetEast;
  xy[2] = xBeamOffsetWest;
  xy[3] = yBeamOffsetWest;
  
  return -1;

}



PHBoolean
PHDchGeometryObject::screenDump()
{
  cout << "CommittingFlag = " << committingFlag << endl;
  if (committingFlag == 0 || committingFlag == 1) cout << "You intend to store G-Info" << endl;
  if (committingFlag == 0 || committingFlag == 2) cout << "You intend to store Wires" << endl;
  if (committingFlag == 0 || committingFlag == 3) cout << "You intend to store Frames" << endl;
  if (committingFlag == 0 || committingFlag == 4) cout << "You intend to store Alphas" << endl;
  if (committingFlag == 0 || committingFlag == 5) cout << "You intend to store Tilts" << endl;
  if (committingFlag == 0 || committingFlag == 6) cout << "You intend to store Beams" << endl;
  if (committingFlag == 0 || committingFlag == 7) cout << "You intend to store Uvtts" << endl;

  //  Print info if you intend to store it...
  if (committingFlag == 0 || committingFlag == 1)
    {

      cout << ncells          << "  ncells          " << endl;
      cout << ngusset         << "  ngusset         " << endl;
      cout << ti_switch       << "  ti_switch       " << endl;
      cout << suppzlength     << "  suppzlength     " << endl;
      cout << innerRadius     << "  innerRadius     " << endl;
      cout << outerRadius     << "  outerRadius     " << endl;
      cout << phibotw         << "  phibotw         " << endl;
      cout << phitopw         << "  phitopw         " << endl;
      cout << phitope         << "  phitope         " << endl;
      cout << phibote         << "  phibote         " << endl;
      cout << planethick      << "  planethick      " << endl;
      cout << uvangle         << "  uvangle         " << endl;
      cout << winthickin      << "  winthickin      " << endl;
      cout << winthickout     << "  winthickout     " << endl;
      cout << supptiside      << "  supptiside      " << endl;
      cout << suppalside      << "  suppalside      " << endl;
      cout << suppzthick      << "  suppzthick      " << endl;
      cout << supptibase      << "  supptibase      " << endl;
      cout << suppalbase      << "  suppalbase      " << endl;
      cout << x1baserad       << "  x1baserad       " << endl;
      cout << x2baserad       << "  x2baserad       " << endl;
      cout << x1basez         << "  x1basez         " << endl;
      cout << x2basez         << "  x2basez         " << endl;
      cout << x1slotthick     << "  x1slotthick     " << endl;
      cout << x2slotthick     << "  x2slotthick     " << endl;
      cout << x1slotz         << "  x1slotz         " << endl;
      cout << x2slotz         << "  x2slotz         " << endl;
      cout << x1suppthick     << "  x1suppthick     " << endl;
      cout << x2suppthick     << "  x2suppthick     " << endl;
      cout << x1suppz         << "  x1suppz         " << endl;
      cout << x2suppz         << "  x2suppz         " << endl;
      cout << x1rextent       << "  x1rextent       " << endl;
      cout << x2rextent       << "  x2rextent       " << endl;
      cout << u1rextent       << "  u1rextent       " << endl;
      cout << v1rextent       << "  v1rextent       " << endl;
      cout << u2rextent       << "  u2rextent       " << endl;
      cout << v2rextent       << "  v2rextent       " << endl;
      cout << u1basez         << "  u1basez         " << endl;
      cout << v1basez         << "  v1basez         " << endl;
      cout << u2basez         << "  u2basez         " << endl;
      cout << v2basez         << "  v2basez         " << endl;
      cout << u1slotz         << "  u1slotz         " << endl;
      cout << v1slotz         << "  v1slotz         " << endl;
      cout << u2slotz         << "  u2slotz         " << endl;
      cout << v2slotz         << "  v2slotz         " << endl;
      cout << u1suppz         << "  u1suppz         " << endl;
      cout << v1suppz         << "  v1suppz         " << endl;
      cout << u2suppz         << "  u2suppz         " << endl;
      cout << v2suppz         << "  v2suppz         " << endl;
      cout << cfibinrad       << "  cfibinrad       " << endl;
      cout << cfiboutrad      << "  cfiboutrad      " << endl;
      cout << propregwidth    << "  propregwidth    " << endl;
      cout << zvsdfactor      << "  zvsdfactor      " << endl;
      cout << guidewiresep    << "  guidewiresep    " << endl;
      cout << angleOfUV1Slots * ToDegree << "  angleOfUV1Slots " << endl;       // rad, fixed angle of UV1 slots
      cout << angleOfUV2Slots * ToDegree << "  angleOfUV2Slots " << endl;       // rad, fixed angle of UV2 slots

      cout << dcwestxo        << "  dcwestxo        " << endl;  // x origin of West
      cout << dcwestyo        << "  dcwestyo        " << endl;
      cout << dcwestzo        << "  dcwestzo        " << endl;
      cout << dceastxo        << "  dceastxo        " << endl;
      cout << dceastyo        << "  dceastyo        " << endl;
      cout << dceastzo        << "  dceastzo        " << endl;
				    
      cout << dcwestxa        << "  dcwestxa        " << endl; // x axis of West
      cout << dcwestya        << "  dcwestya        " << endl;
      cout << dcwestza        << "  dcwestza        " << endl;
      cout << dceastxa        << "  dceastxa        " << endl;
      cout << dceastya        << "  dceastya        " << endl;
      cout << dceastza        << "  dceastza        " << endl;
      
      for (int plane = 0; plane < numberOfPlanes;plane++)
	{
	  cout << radiusAtExtremZ[plane] << "  radiusAtExtremZ " << endl;
	}
      
    }


  //  Print wires if you intend to store them...
  if (committingFlag == 0 || committingFlag == 2)
    {
      
      // the entry are --> basepoint, wire direction, drift direction
      int global, arm, plane, cell;
      float bnx, bny, bnz, bsx, bsy, bsz, ddnx, ddny, ddnz, ddsx, ddsy, ddsz;
      
      for (int i = 0; i < 12800/2; i++)
	{
	  dchaddress->getGlobalIndex()->setValue(i);                // set the value of the global index
	  dchaddress->setGlobalIndex(dchaddress->getGlobalIndex()); // update all the indices 
	  arm   = dchaddress->getArm()->getValue();
	  plane = dchaddress->getPlane()->getValue();
	  cell  = dchaddress->getCell()->getValue();
	  
	  global = i;
	  
	  bnx  = wireBasepointNorth[arm][plane][cell].getX();
	  bny  = wireBasepointNorth[arm][plane][cell].getY();
	  bnz  = wireBasepointNorth[arm][plane][cell].getZ();
	  bsx  = wireBasepointSouth[arm][plane][cell].getX();
	  bsy  = wireBasepointSouth[arm][plane][cell].getY();
	  bsz  = wireBasepointSouth[arm][plane][cell].getZ();
	  ddnx = wireDriftDirectionNorth[arm][plane][cell].getX();
	  ddny = wireDriftDirectionNorth[arm][plane][cell].getY();
	  ddnz = wireDriftDirectionNorth[arm][plane][cell].getZ();
	  ddsx = wireDriftDirectionSouth[arm][plane][cell].getX();
	  ddsy = wireDriftDirectionSouth[arm][plane][cell].getY();
	  ddsz = wireDriftDirectionSouth[arm][plane][cell].getZ();
	  
	  
	  cout << " " << global; 
	  cout << " " << arm   ;
	  cout << " " << plane ;
	  cout << " " << cell  ;
	  cout << " " << bnx   ;
	  cout << " " << bny   ;
	  cout << " " << bnz   ;
	  cout << " " << bsx   ;
	  cout << " " << bsy   ;
	  cout << " " << bsz   ;
	  cout << " " << ddnx  ;
	  cout << " " << ddny  ;
	  cout << " " << ddnz  ;
	  cout << " " << ddsx  ;
	  cout << " " << ddsy  ;
	  cout << " " << ddsz  ; 
	  cout << endl;
	  
	}
    }
  
  
  //  Print alphas if you intend to store them...
  if (committingFlag == 0 || committingFlag == 3) {
    cout << "FRAMES: index value" << endl;
    for (int i=0; i<24; i++) {
      cout << tmpValueFrame[i]   << " " << parameterFrame[i];
      cout << endl;
    }
  }

  //  Print alphas if you intend to store them...
  if (committingFlag == 0 || committingFlag == 4) {
    cout << "ALPHA: Set Arm Side Cell Alpha" << endl;
    for (int i=0; i<2; i++) {
      for (int j=0; j<2; j++) {
	for (int k=0; k<2; k++) {
	  for (int l=0; l<80; l++) {
	    cout << i  << " ";
	    cout << j   << " ";
	    cout << k  << " ";
	    cout << l  << " ";
	    cout << A0[i][j][k][l] << endl;
	  }
	}
      }
    }
  }


  //  Print tilts if you intend to save them...
  if (committingFlag == 0 || committingFlag == 5) {
    cout << "TILT: Arm Cell x1Slope x1Offset x2Slope x2Offset" << endl;
    for (int i=0; i<2; i++) {
      for (int j=0; j<80; j++) {
	cout << i   << " ";
	cout << j  << " ";
	cout << x1Slope [i][j]  << " ";
	cout << x1Offset[i][j]  << " ";
	cout << x2Slope [i][j]  << " ";
	cout << x2Offset[i][j]  << endl;
      }
    }
  }

  //  Print beams if you intend to save them...
  if (committingFlag == 0 || committingFlag == 6) {
    cout << "  xOffsetEast " << xBeamOffsetEast;
    cout << "  yOffsetEast " << yBeamOffsetEast;
    cout << "  xOffsetWest " << xBeamOffsetWest;
    cout << "  yOffsetWest " << yBeamOffsetWest;
    cout << endl;
  }

  //  Print uvtts if you intend to save them...
  if (committingFlag == 0 || committingFlag == 7) {
    cout << "UVTT: Arm Wire uvSlope uvOffset" << endl;
    for (int i=0; i<2; i++) {
      for (int j=0; j<16; j++) {
	cout << i   << " ";
	cout << j  << " ";
	cout << uvSlope [i][j]  << " ";
	cout << uvOffset[i][j]  << " ";
	cout << endl;
      }
    }
  }

  return True;
}

