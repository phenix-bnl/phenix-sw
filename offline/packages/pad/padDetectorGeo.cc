// Class: padDetectorGeo (implementation)
// Created by: Jeffery T. Mitchell
// Description: Class that generates the pad chamber geometry from
//              either a default, or from the information in the database
// last update: 04/15/03 simulation geometry in OBJY database, Indrani Ojha
// last update: 12/07/04 simulation geometry in PG database, Debsankar M.
//-------------------------------------------------------------------------
#include <iostream>
#include <fstream>
#include <cmath>

#include <padDetectorGeo.hh>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <dPadGeomWrapper.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbPadGeoCham.hh>
#include <PdbPadGeoPar.hh>
#include <PHGeometry.h>
#include <PdbPadSimGeoChamV2.hh>
#include <PdbPadSimGeoParV1.hh>

using namespace std;
using namespace PHGeometry;

// Default Constructor for padDetectorGeo
padDetectorGeo::padDetectorGeo()
{
  int i, j;

  Verbose = 0;
  for (i = 0; i < 2; i++)
    {
      for (j = 0; j < PC1MAXSECTPERARM; j++)
        {
          pc1Active[i][j] = 0;
        }
    }
  for (i = 0; i < 2; i++)
    {
      for (j = 0; j < CGLMAXSECTPERARM; j++)
        {
          pc2Active[i][j] = 0;
          pc3Active[i][j] = 0;
        }
    }
  xyz0.setX(0.0);
  xyz0.setY(0.0);
  xyz0.setZ(0.0);
  for (i = 0; i < 2; i++)
    {
      Theta0[i] = 0.0;
    }
  pc1Radius = 0.0;
  pc2Radius = 0.0;
  pc3Radius = 0.0;
  pc1ZWidth = 0.0;
  pc2ZWidth = 0.0;
  pc3ZWidth = 0.0;
  Tsearch.setToSystemTime();
  //TUpdateStart.setToSystemTime();
  TUpdateStart.set(2001, 5, 1, 0, 0, 0, 0); // beginning of run2
  TUpdateStop.setToFarFuture();
  // setup default frames
  PHFrame XYZ;  //this is a default frame
  phenixFrame = XYZ;
  westFrame = XYZ;
  eastFrame = XYZ;
  for (int iarm = 0;iarm < 2;iarm++)
    {
      for (int isector = 0;isector < PC1MAXSECTPERARM;isector++)
        {
          pc1SectorFrame[iarm][isector] = XYZ;
        }
      for (int isector = 0;isector < CGLMAXSECTPERARM;isector++)
        {
          pc2SectorFrame[iarm][isector] = XYZ;
          pc3SectorFrame[iarm][isector] = XYZ;
          for (int iside = 0;iside < 2;iside++)
            {
              pc2ChamberFrame[iarm][isector][iside] = XYZ;
              pc3ChamberFrame[iarm][isector][iside] = XYZ;
            }
        }
    }
}

// Destructor for padDetectorGeo
padDetectorGeo::~padDetectorGeo()
{
}

// Build the active PC1 detector geometries
void
padDetectorGeo::BuildPC1Geo(short arm)
{
  // Build this as a regular polygon starting at the reference
  // theta for sector 0.  Generate a plane for all sectors.
  int iside;
  short nSides = PC1MAXSECTPERARM * 4;  // number of sides in the polygon
  double dTheta = 0.0;  // angle spanned by each sector
  PHAngle ThetaLow;     // The lower angle of each sector
  PHAngle ThetaHigh;    // The upper angle of each sector
  double cosTheta;      // cosine of the theta span
  double rOuter;        // The outer inscribed radius
  PHPoint xP0, xP1, xP2;  // coordinates of panel ref points

  // Make sure the input is OK
  if (arm < 0 || arm >= 2)
    {
      return;
    }
  if (nSides == 0)
    {
      return;
    }
  // Calculate PC1-wide variables
  dTheta = (2.0 * M_PI) / nSides;
  cosTheta = cos(dTheta / 2.0);
  rOuter = 0.0;
  if (cosTheta != 0.0)
    rOuter = pc1Radius / cosTheta;
   // Loop over each sector
  for (iside = 0; iside < PC1MAXSECTPERARM; iside++)
    {
      ThetaLow = double(Theta0[arm]) + iside * dTheta;
      ThetaHigh = double(Theta0[arm]) + (iside + 1) * dTheta;
      // p0 of the PHPanel will be at ThetaLow and -z
      xP0.setX(rOuter*cos(ThetaLow));
      xP0.setY(rOuter*sin(ThetaLow));
      xP0.setZ( -pc1ZWidth);
      // p1 of the PHPanel will be at ThetaHigh and -z
      xP1.setX(rOuter*cos(ThetaHigh));
      xP1.setY(rOuter*sin(ThetaHigh));
      xP1.setZ( -pc1ZWidth);
      // p2 of the PHPanel will be at ThetaLow and +z
      xP2.setX(rOuter*cos(ThetaLow));
      xP2.setY(rOuter*sin(ThetaLow));
      xP2.setZ(pc1ZWidth);
      // Store the panel information
      PHPanel tempPanel(xP0, xP1, xP2);
      pc1Sectors[arm][iside] = tempPanel;
    }
}

short
padDetectorGeo::get_pc1Geo(short arm, PHPanel outPlane[])
{
  short sect, nSector;
  nSector = 0;
  for (sect = 0; sect < PC1MAXSECTPERARM; sect++)
    {
      if (pc1Active[arm][sect] == 1)
        {
          outPlane[nSector] = pc1Sectors[arm][sect];
          nSector++;
        }
    }
  return nSector;
}

// Build the active PC2 detector geometries
void
padDetectorGeo::BuildPC2Geo(short arm)
{
  // Build this as a regular polygon starting at the reference
  // theta for sector 0.  Generate a plane for all sectors.
  int iside;
  short nSides = CGLMAXSECTPERARM * 4;  // number of sides in the polygon
  double dTheta = 0.0;  // angle spanned by each sector
  PHAngle ThetaLow;     // The lower angle of each sector
  PHAngle ThetaHigh;    // The upper angle of each sector
  double cosTheta;      // cosine of the theta span
  double rOuter;        // The outer inscribed radius
  PHPoint xP0, xP1, xP2;  // coordinates of panel ref points
  // Make sure the input is OK
  if ((arm < 0) || (arm >= 2))
    {
      return;
    }
  if (nSides == 0)
    {
      return;
    }
  // Calculate PC2-wide variables
  dTheta = (2.0 * M_PI) / nSides;
  cosTheta = cos(dTheta / 2.0);
  rOuter = 0.0;
  if (cosTheta != 0.0)
    {
      rOuter = pc2Radius / cosTheta;
    }
  // Loop over each sector
  for (iside = 0; iside < CGLMAXSECTPERARM; iside++)
    {
      ThetaLow = double(Theta0[arm]) + iside * dTheta;
      ThetaHigh = double(Theta0[arm]) + (iside + 1) * dTheta;
      // p0 of the PHPanel will be at ThetaLow and -z
      xP0.setX(rOuter*cos(ThetaLow));
      xP0.setY(rOuter*sin(ThetaLow));
      xP0.setZ( -pc2ZWidth);
      // p1 of the PHPanel will be at ThetaHigh and -z
      xP1.setX(rOuter*cos(ThetaHigh));
      xP1.setY(rOuter*sin(ThetaHigh));
      xP1.setZ( -pc2ZWidth);
      // p2 of the PHPanel will be at ThetaLow and +z
      xP2.setX(rOuter*cos(ThetaLow));
      xP2.setY(rOuter*sin(ThetaLow));
      xP2.setZ(pc2ZWidth);
      // Store the panel information
      PHPanel* tempPanel = new PHPanel(xP0, xP1, xP2);
      pc2Sectors[arm][iside] = *tempPanel;
      delete tempPanel;
    }
}

short
padDetectorGeo::get_pc2Geo(short arm, PHPanel outPlane[])
{
  short sect, side, module;
  module = 0;
  if (isFromDatabase())
    {
      for (side = 0; side < 2; side++)
        {
          for (sect = 0; sect < CGLMAXSECTPERARM; sect++)
            {
              if (pc2Active[arm][sect] == 1)
                {
                  outPlane[module] = pc2Chambers[arm][sect][side];
                  module++;
                }
            }
        }
    }
  else
    { // from ascii file
      for (sect = 0; sect < CGLMAXSECTPERARM; sect++)
        {
          if (pc2Active[arm][sect] == 1)
            {
              outPlane[module] = pc2Sectors[arm][sect];
              module++;
            }
        }
    }
  return module;
}

// Build the active PC3 detector geometries
void
padDetectorGeo::BuildPC3Geo(short arm)
{
  // Build this as a regular polygon starting at the reference
  // theta for sector 0.  Generate a plane for all sectors.
  int iside;
  short nSides = CGLMAXSECTPERARM * 4;  // number of sides in the polygon
  double dTheta = 0.0;  // angle spanned by each sector
  PHAngle ThetaLow;     // The lower angle of each sector
  PHAngle ThetaHigh;    // The upper angle of each sector
  double cosTheta;      // cosine of the theta span
  double rOuter;        // The outer inscribed radius
  PHPoint xP0, xP1, xP2;  // coordinates of panel ref points
  // Make sure the input is OK
  if (arm < 0 || arm >= 2)
    {
      return;
    }
  if (nSides == 0)
    {
      return;
    }
  // Calculate PC3-wide variables
  dTheta = (2.0 * M_PI) / nSides;
  cosTheta = cos(dTheta / 2.0);
  rOuter = 0.0;
  if (cosTheta != 0.0)
    {
      rOuter = pc3Radius / cosTheta;
    }
  // Loop over each sector
  for (iside = 0; iside < CGLMAXSECTPERARM; iside++)
    {
      ThetaLow = double(Theta0[arm]) + iside * dTheta;
      ThetaHigh = double(Theta0[arm]) + (iside + 1) * dTheta;
      // p0 of the PHPanel will be at ThetaLow and -z
      xP0.setX(rOuter*cos(ThetaLow));
      xP0.setY(rOuter*sin(ThetaLow));
      xP0.setZ( -pc3ZWidth);
      // p1 of the PHPanel will be at ThetaHigh and -z
      xP1.setX(rOuter*cos(ThetaHigh));
      xP1.setY(rOuter*sin(ThetaHigh));
      xP1.setZ( -pc3ZWidth);
      // p2 of the PHPanel will be at ThetaLow and +z
      xP2.setX(rOuter*cos(ThetaLow));
      xP2.setY(rOuter*sin(ThetaLow));
      xP2.setZ(pc3ZWidth);
      // Store the panel information
      PHPanel* tempPanel = new PHPanel(xP0, xP1, xP2);
      pc3Sectors[arm][iside] = *tempPanel;
      delete tempPanel;
    }
}

short
padDetectorGeo::get_pc3Geo(short arm, PHPanel outPlane[])
{
  short sect, side, module;
  module = 0;
  if (isFromDatabase())
    {
      for (side = 0; side < 2; side++)
        {
          for (sect = 0; sect < CGLMAXSECTPERARM; sect++)
            {
              if (pc3Active[arm][sect] == 1)
                {
                  outPlane[module] = pc3Chambers[arm][sect][side];
                  module++;
                }
            }
        }
    }
  else
    { // from ascii file
      for (sect = 0; sect < CGLMAXSECTPERARM; sect++)
        {
          if (pc3Active[arm][sect] == 1)
            {
              outPlane[module] = pc3Sectors[arm][sect];
              module++;
            }
        }
    }
  return module;
}

// Build all geometries for both arms
void
padDetectorGeo::BuildAllGeo()
{
  BuildPC1Geo(0);
  BuildPC1Geo(1);
  BuildPC2Geo(0);
  BuildPC2Geo(1);
  BuildPC3Geo(0);
  BuildPC3Geo(1);
}

// Fetch the geometry information contained in dPadGeom
void
padDetectorGeo::Fetch_dPadGeom(PHCompositeNode* topNode)
{
  int i;
  int nActive;
  double armAngle;
  PHAngle addAngle;
  // Fetch the pointer to dPadGeom from the topNode
  typedef PHIODataNode<dPadGeomWrapper> dPadGeomNode;
  PHNodeIterator ini(topNode);
  dPadGeomNode* geomNode = static_cast<dPadGeomNode*>(ini.findFirst("PHIODataNode", "dPadGeom"));
  dPadGeomWrapper* padGeom = geomNode->getData();
  // Set pad chamber specific quantities
  for (i = 0; i < 3; i++)
    {
      padGeom->set_pdxoff(i, 0, xOffset[i]);
      padGeom->set_pdzoff(i, 0, zOffset[i]);
      padGeom->set_pdgas(i, 0, gasAtten[i]);
      padGeom->set_aasep(i, 0, anodeSpacing[i]);
      padGeom->set_pxlen(i, 0, pixelLength[i]);
      padGeom->set_wside(i, 0, sideWidth[i]);
      padGeom->set_wcent(i, 0, centerWidth[i]);
      padGeom->set_pxsep(i, 0, pixelSpacing[i]);
      padGeom->set_clsep(i, 0, cellSpacing[i]);
      padGeom->set_zgap(i, 0, z0Gap[i]);
      padGeom->set_npdwr(i, 0, nWiresPerSect[i]);
      padGeom->set_npdx(i, 0, nPadsAcrossWire[i]);
      padGeom->set_npdz(i, 0, nPadsAlongWire[i]);
    }
  // Set the inner inscribed radius
  padGeom->set_inradius(0, 0, pc1Radius);
  padGeom->set_inradius(1, 0, pc2Radius);
  padGeom->set_inradius(2, 0, pc3Radius);
  // Set the number of sectors per arm
  // Only look at arm 0 as default
  nActive = 0;
  for (i = 0; i < PC1MAXSECTPERARM; i++)
    {
      if (pc1Active[0][i] == 1)
        nActive++;
    }
  padGeom->set_sectperarm(0, 0, nActive);
  padGeom->set_npdsec(0, 0, PC1MAXSECTPERARM*2);
  nActive = 0;
  for (i = 0; i < CGLMAXSECTPERARM; i++)
    {
      if (pc2Active[0][i] == 1)
        {
	  nActive++;
	}
    }
  padGeom->set_sectperarm(1, 0, nActive);
  padGeom->set_npdsec(1, 0, CGLMAXSECTPERARM*2);
  nActive = 0;
  for (i = 0; i < CGLMAXSECTPERARM; i++)
    {
      if (pc3Active[0][i] == 1)
        {
	  nActive++;
	}
    }
  padGeom->set_sectperarm(2, 0, nActive);
  padGeom->set_npdsec(2, 0, CGLMAXSECTPERARM*2);
  // Set global properties.
  // Adjust the angle so that it lies in the range [-90.0,270.0],
  // which is what modules using dPadGeom expect
  addAngle = Theta0[0];
  armAngle = addAngle.degree();
  if (armAngle > 270.0)
    {
      armAngle -= 360.0;
    }
  padGeom->set_phibotw(0, armAngle);
  addAngle += 1.570796;  // add 90 degrees
  armAngle = addAngle.degree();
  if (armAngle > 270.0)
    {
      armAngle -= 360.0;
    }
  padGeom->set_phitopw(0, armAngle);
  addAngle = Theta0[1];
  armAngle = addAngle.degree();
  if (armAngle > 270.0)
    {
      armAngle -= 360.0;
    }
  padGeom->set_phitope(0, armAngle);
  addAngle += 1.570796;
  armAngle = addAngle.degree();
  if (armAngle > 270.0)
    {
      armAngle -= 360.0;
    }
  padGeom->set_phibote(0, armAngle);
}

// Fetch the geometry information from the database
PHBoolean
padDetectorGeo::FetchFromDatabase(PHTimeStamp TS)
{
  double radius;
  // Set the logical database variable
  fromDatabase = True;
  // Set the current time stamp (method defined in PadBasicObject.hh)
  setTimeStamp(TS);
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (application->startRead())
    {
      ///////////////////////////////
      // Chamber geometry database //
      ///////////////////////////////
      setCalibName("geom.pad.geocham");
      setBankNumber(4104);
      setBankID(BankNumber);
      PdbCalBank *padBank = bankManager->fetchBank("PdbPadGeoChamBank",
						   BankID,
						   "geom.pad.geocham",
						   Tsearch);
      if (padBank)
        {
          // The number of active sectors for each pad chamber
          short nSectPc1, nSectPc2, nSectPc3;
          nSectPc1 = 2 * PC1MAXSECTPERARM;
          nSectPc2 = nSectPc3 = 2 * CGLMAXSECTPERARM;
          short iarm, iside, isector;
          double x, y, z;
          PHPoint northUpPoint, southUpPoint, northDownPoint, southDownPoint;
          PHPoint panelPoint[4];
          PdbPadGeoCham *sectorobj;
          // Read in the sectors for PC1 and store the data members
          for (int i = 0; i < nSectPc1; i++)
            {
              // Read in the indexing for this sector
              sectorobj = (PdbPadGeoCham*) & (padBank->getEntry(i));
              iarm = sectorobj->getArm();
              iside = sectorobj->getSide();
              isector = sectorobj->getSector();
              if ((iarm < 2) && (isector < PC1MAXSECTPERARM))
                {
                  pc1Active[iarm][isector] = 1;
                }
              else
                {
                  cout << "padDetectorGeo::FetchFromDatabase-F:  PC1 data format error.\n";
                  return False;
                }
              // Read in the coordinates
              southDownPoint = sectorobj->getSouthDownPoint(); // P0
              x = southDownPoint.getX();
              y = southDownPoint.getY();
              z = southDownPoint.getZ();
              panelPoint[0].setX(x);
              panelPoint[0].setY(y);
              panelPoint[0].setZ(z);
              southUpPoint = sectorobj->getSouthUpPoint(); // P1
              x = southUpPoint.getX();
              y = southUpPoint.getY();
              z = southUpPoint.getZ();
              panelPoint[1].setX(x);
              panelPoint[1].setY(y);
              panelPoint[1].setZ(z);
              northDownPoint = sectorobj->getNorthDownPoint(); // P2
              x = northDownPoint.getX();
              y = northDownPoint.getY();
              z = northDownPoint.getZ();
              panelPoint[2].setX(x);
              panelPoint[2].setY(y);
              panelPoint[2].setZ(z);
              northUpPoint = sectorobj->getNorthUpPoint(); // P3
              x = northUpPoint.getX();
              y = northUpPoint.getY();
              z = northUpPoint.getZ();
              panelPoint[3].setX(x);
              panelPoint[3].setY(y);
              panelPoint[3].setZ(z);
              // Check the deviation between the calculated panelPoint[3] and the survey point
              // double xx = panelPoint[3].getX();
              // double yy = panelPoint[3].getY();
              // double zz = panelPoint[3].getZ();
              // r = sqrt((x-xx)*(x-xx) + (y-yy)*(y-yy) + (z-zz)*(z-zz));
              // cout << "Offset from P3: " << r << endl;
              // Store the PHPanel object
              PHPanel pc1Panel(panelPoint[0], 
			       panelPoint[1], 
			       panelPoint[2]);
              pc1Sectors[iarm][isector] = pc1Panel;
            }
          // Read in the sectors for PC2 and store the data members
          for (int i = nSectPc1; i < (nSectPc1 + 2*nSectPc2); i++)
            {
              // Read in the indexing for this sector
              sectorobj = (PdbPadGeoCham*) & (padBank->getEntry(i));
              iarm = sectorobj->getArm();
              iside = sectorobj->getSide();
              isector = sectorobj->getSector();
              if (iside == 0)
                {
                  if ((iarm < 2) && (isector < CGLMAXSECTPERARM))
                    {
                      pc2Active[iarm][isector] = 1;
                    }
                  else
                    {
                      cout << "padDetectorGeo::FetchFromDatabase-F:  PC2 data format error.\n";
                      return False;
                    }
                }
              // Read in the coordinates
              southDownPoint = sectorobj->getSouthDownPoint(); // P0
              x = southDownPoint.getX();
              y = southDownPoint.getY();
              z = southDownPoint.getZ();
              panelPoint[0].setX(x);
              panelPoint[0].setY(y);
              panelPoint[0].setZ(z);
              southUpPoint = sectorobj->getSouthUpPoint(); // P1
              x = southUpPoint.getX();
              y = southUpPoint.getY();
              z = southUpPoint.getZ();
              panelPoint[1].setX(x);
              panelPoint[1].setY(y);
              panelPoint[1].setZ(z);
              northDownPoint = sectorobj->getNorthDownPoint(); // P2
              x = northDownPoint.getX();
              y = northDownPoint.getY();
              z = northDownPoint.getZ();
              panelPoint[2].setX(x);
              panelPoint[2].setY(y);
              panelPoint[2].setZ(z);
              northUpPoint = sectorobj->getNorthUpPoint(); // P3
              x = northUpPoint.getX();
              y = northUpPoint.getY();
              z = northUpPoint.getZ();
              panelPoint[3].setX(x);
              panelPoint[3].setY(y);
              panelPoint[3].setZ(z);
              // Store the PHPanel object
              PHPanel pc2Panel(panelPoint[0], panelPoint[1], panelPoint[2]);
              pc2Chambers[iarm][isector][iside] = pc2Panel;
            }
          // Read in the sectors for PC3 and store the data members
          for (int i = (nSectPc1 + 2 * nSectPc2); i < (nSectPc1 + 2*nSectPc2 + 2*nSectPc3); i++)
            {
              // Read in the indexing for this sector
              sectorobj = (PdbPadGeoCham*) & (padBank->getEntry(i));
              iarm = sectorobj->getArm();
              iside = sectorobj->getSide();
              isector = sectorobj->getSector();
              if (iside == 0)
                {
                  if ((iarm < 2) && (isector < CGLMAXSECTPERARM))
                    {
                      pc3Active[iarm][isector] = 1;
                    }
                  else
                    {
                      cout << "padDetectorGeo::FetchFromDatabase-F:  PC3 data format error.\n";
                      return False;
                    }
                }
              // Read in the coordinates
              southDownPoint = sectorobj->getSouthDownPoint(); // P0
              x = southDownPoint.getX();
              y = southDownPoint.getY();
              z = southDownPoint.getZ();
              panelPoint[0].setX(x);
              panelPoint[0].setY(y);
              panelPoint[0].setZ(z);
              southUpPoint = sectorobj->getSouthUpPoint(); // P1
              x = southUpPoint.getX();
              y = southUpPoint.getY();
              z = southUpPoint.getZ();
              panelPoint[1].setX(x);
              panelPoint[1].setY(y);
              panelPoint[1].setZ(z);
              northDownPoint = sectorobj->getNorthDownPoint(); // P2
              x = northDownPoint.getX();
              y = northDownPoint.getY();
              z = northDownPoint.getZ();
              panelPoint[2].setX(x);
              panelPoint[2].setY(y);
              panelPoint[2].setZ(z);
              northUpPoint = sectorobj->getNorthUpPoint(); // P3
              x = northUpPoint.getX();
              y = northUpPoint.getY();
              z = northUpPoint.getZ();
              panelPoint[3].setX(x);
              panelPoint[3].setY(y);
              panelPoint[3].setZ(z);
              // Store the PHPanel object
              PHPanel pc3Panel (panelPoint[0], panelPoint[1], panelPoint[2]);
              pc3Chambers[iarm][isector][iside] = pc3Panel;
            }
	  delete padBank;
        }
      else
        {
          cout << "Error: bankManager returned zero-pointer" << endl;
        }
      /////////////////////////////////
      // Geometry parameter database //
      /////////////////////////////////
      setCalibName("geom.pad.geopar");
      setBankNumber(4105);
      setBankID(BankNumber);
      PdbCalBank *padParBank = bankManager->fetchBank("PdbPadGeoParBank", BankID, "geom.pad.geopar", Tsearch);
      // Test access to the header
      if (padParBank)
        {
          PdbPadGeoPar *padgeoparobj;
          for (int pc = 0; pc < 3; pc++)
            {
              padgeoparobj = (PdbPadGeoPar*) & (padParBank->getEntry(pc));
              gasAtten[pc] = padgeoparobj->get_gasAtten();
              anodeSpacing[pc] = padgeoparobj->get_anodeSpacing();
              pixelLength[pc] = padgeoparobj->get_pixelLength();
              sideWidth[pc] = padgeoparobj->get_sidePixelWidth();
              centerWidth[pc] = padgeoparobj->get_centerPixelWidth();
              pixelSpacing[pc] = padgeoparobj->get_pixelSpacing();
              cellSpacing[pc] = padgeoparobj->get_cellSpacing();
              nActiveSectors[pc] = padgeoparobj->get_nActiveSectors();
              nSectPerArm[pc] = padgeoparobj->get_nSectPerArm();
              nWiresPerSect[pc] = padgeoparobj->get_nWiresPerSect();
              nPadsAcrossWire[pc] = padgeoparobj->get_nPadsAcrossWire();
              nPadsAlongWire[pc] = padgeoparobj->get_nPadsAlongWire();
              z0Gap[pc] = padgeoparobj->get_z0Gap();
              xOffset[pc] = padgeoparobj->get_xOffset();
              zOffset[pc] = padgeoparobj->get_zOffset();
              radius = padgeoparobj->get_pcRadius();
              if (pc == 0)
                {
		  pc1Radius = radius;
		}
              else if (pc == 1)
                {
		  pc2Radius = radius;
		}
              else
                {
		  pc3Radius = radius;
		}
              phiTopEast[pc] = padgeoparobj->get_phiTopEast();
              phiTopWest[pc] = padgeoparobj->get_phiTopWest();
              phiBottomEast[pc] = padgeoparobj->get_phiBottomEast();
              phiBottomWest[pc] = padgeoparobj->get_phiBottomWest();
            }
	  delete padParBank;
        }
      //    application->commit()
    }
  else
    {
      application->abort();
      cerr << "padDetectorGeo::FetchFromDatabase() ERROR: Transaction aborted." << endl;
      return False;
    }
  iFlag = 0;
  return True;
}

// Fetch the geometry information from simulation database
PHBoolean
padDetectorGeo::FetchFromSimDatabase(PHTimeStamp TS)
{// xx
 
  fromSimDatabase = True;   // this flag is not yet in use, but could be in the future (CFM)
  fromDatabase = False;      // PadRec uses only the isFromDatabase method at this time (12/31/2003)
  // Set the time stamp (method defined in PadBasicObject.hh)
  setTimeStamp(TS);
  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (application->startRead())
    { // xxx
      
    // Simulation geometry database //
      setCalibName("geom.pad.simgeocham");
      setBankNumber(4110);
      setBankID(BankNumber);
      PdbCalBank *padBank = bankManager->fetchBank("PdbPadSimGeoChamV2Bank",
						   BankID,
						   "geom.pad.simgeocham",
						   TS);
      if (padBank)
        {
          // The number of active sectors for each pad chamber
          short nSectPc1, nSectPc2, nSectPc3;
          nSectPc1 = 2 * PC1MAXSECTPERARM;
          nSectPc2 = nSectPc3 = 2 * CGLMAXSECTPERARM;
       
	  int arm, sect; 
	  double p0[3],p1[3],p2[3],p3[3];
          PHPoint panelPoint[4];
          PdbPadSimGeoChamV2 *sectorobj;
	  int chamber = 0;
          // Read sectors for PC1 
          for (int i = 0; i < nSectPc1; i++)
            {//pc1
              sectorobj = (PdbPadSimGeoChamV2*) & (padBank->getEntry(chamber));
              arm = sectorobj->get_arm();
              sect = sectorobj->get_sect();
		    for(int j=0;j<3;j++){
              // Read in the indexing for this sector

	      p0[j] = sectorobj->get_point1(j);
	      p1[j] = sectorobj->get_point2(j);
	      p2[j] = sectorobj->get_point3(j);
	      p3[j] = sectorobj->get_point4(j);
		    }
	      
	      if ((arm < 2) && (sect <PC1MAXSECTPERARM ))
	      {
	          pc1Active[arm][sect] = 1;
		  panelPoint[0].setX(p0[0]);
		  panelPoint[0].setY(p0[1]);
		  panelPoint[0].setZ(p0[2]);

		  panelPoint[1].setX(p1[0]);
		  panelPoint[1].setY(p1[1]);
		  panelPoint[1].setZ(p1[2]);

		  panelPoint[2].setX(p2[0]);
		  panelPoint[2].setY(p2[1]);
		  panelPoint[2].setZ(p2[2]);

		  panelPoint[3].setX(p3[0]);
		  panelPoint[3].setY(p3[1]);
		  panelPoint[3].setZ(p3[2]);
 
		  PHPanel pc1Panel(panelPoint[0], 
				   panelPoint[1], 
				   panelPoint[2]); 
		  pc1Sectors[arm][sect] = pc1Panel;
		  chamber++;
		   }
              else
                {
                  cout << "padDetectorGeo::FetchFromSimDatabase-F:  PC1 data format error.\n";
                  return False;
                }
		  
	    }
	   

          // Read sectors for PC2 
          for (int i = 0; i < nSectPc2; i++)
            {//pc2
              sectorobj = (PdbPadSimGeoChamV2*) & (padBank->getEntry(chamber));
              arm = sectorobj->get_arm();
              sect = sectorobj->get_sect();
		    for(int j=0;j<3;j++){
              // Read in the indexing for this sector

	      p0[j] = sectorobj->get_point1(j);
	      p1[j] = sectorobj->get_point2(j);
	      p2[j] = sectorobj->get_point3(j);
	      p3[j] = sectorobj->get_point4(j);
		    }
	      
	      if ((arm < 2) && (sect <CGLMAXSECTPERARM ))
	      {
	          pc2Active[arm][sect] = 1;
		  panelPoint[0].setX(p0[0]);
		  panelPoint[0].setY(p0[1]);
		  panelPoint[0].setZ(p0[2]);

		  panelPoint[1].setX(p1[0]);
		  panelPoint[1].setY(p1[1]);
		  panelPoint[1].setZ(p1[2]);

		  panelPoint[2].setX(p2[0]);
		  panelPoint[2].setY(p2[1]);
		  panelPoint[2].setZ(p2[2]);

		  panelPoint[3].setX(p3[0]);
		  panelPoint[3].setY(p3[1]);
		  panelPoint[3].setZ(p3[2]);
 
		  PHPanel pc2Panel (panelPoint[0], 
				    panelPoint[1], 
				    panelPoint[2]); 
		  pc2Sectors[arm][sect] = pc2Panel;
		  chamber++;
		   }
              else
                {
                  cout << "padDetectorGeo::FetchFromSimDatabase-F:  PC1 data format error.\n";
                  return False;
                }
		  
	    }
	
    

	   

          // Read sectors for PC3 
          for (int i = 0; i < nSectPc3; i++)
            {//pc3
              sectorobj = (PdbPadSimGeoChamV2*) & (padBank->getEntry(chamber));
              arm = sectorobj->get_arm();
              sect = sectorobj->get_sect();
		    for(int j=0;j<3;j++){
              // Read in the indexing for this sector

	      p0[j] = sectorobj->get_point1(j);
	      p1[j] = sectorobj->get_point2(j);
	      p2[j] = sectorobj->get_point3(j);
	      p3[j] = sectorobj->get_point4(j);
		    }
	      
	      if ((arm < 2) && (sect <CGLMAXSECTPERARM ))
	      {
	          pc3Active[arm][sect] = 1;
		  panelPoint[0].setX(p0[0]);
		  panelPoint[0].setY(p0[1]);
		  panelPoint[0].setZ(p0[2]);

		  panelPoint[1].setX(p1[0]);
		  panelPoint[1].setY(p1[1]);
		  panelPoint[1].setZ(p1[2]);

		  panelPoint[2].setX(p2[0]);
		  panelPoint[2].setY(p2[1]);
		  panelPoint[2].setZ(p2[2]);

		  panelPoint[3].setX(p3[0]);
		  panelPoint[3].setY(p3[1]);
		  panelPoint[3].setZ(p3[2]);
 
		  PHPanel pc3Panel (panelPoint[0], 
				    panelPoint[1], 
				    panelPoint[2]); 
		  pc3Sectors[arm][sect] = pc3Panel;
		  chamber++;
		   }
              else
                {
                  cout << "padDetectorGeo::FetchFromSimDatabase-F:  PC1 data format error.\n";
                  return False;
                }
		  
	    }
	   

	  delete padBank;	  
	}
            
// Now fetching parameter inforamtion
      setCalibName("geom.pad.simgeopar");
      setBankNumber(4111);
      setBankID(BankNumber);
      PdbCalBank *padSimParBank = bankManager->fetchBank("PdbPadSimGeoParV1Bank", BankID, "geom.pad.simgeopar", TS);
      // Test access to the header
      if (padSimParBank)
	{

	//Create a sim geometry parameter object
	  PdbPadSimGeoParV1 *padsimgeoparobj;
	  for (int i=0; i<3; i++)
	    {
	     
	      padsimgeoparobj = (PdbPadSimGeoParV1*) & padSimParBank->getEntry(i);
	      //set parameters
	      z0Gap[i] = padsimgeoparobj->get_z0Gap();
	      xOffset[i] = padsimgeoparobj->get_xOffset();
	      zOffset[i] = padsimgeoparobj->get_zOffset();
	      gasAtten[i] = padsimgeoparobj->get_gasAtten();
	      anodeSpacing[i] = padsimgeoparobj->get_anodeSpacing();
	      pixelLength[i] = padsimgeoparobj->get_pixelLength();
	      sideWidth[i] = padsimgeoparobj->get_sideWidth();
	      centerWidth[i] = padsimgeoparobj->get_centerWidth();
	      pixelSpacing[i] = padsimgeoparobj->get_pixelSpacing();
	      cellSpacing[i] = padsimgeoparobj->get_cellSpacing();
	      nWiresPerSect[i] = padsimgeoparobj->get_nWiresPerSect();
	      nPadsAcrossWire[i] = padsimgeoparobj->get_nPadsAcrossWire();
	      nPadsAlongWire[i] = padsimgeoparobj->get_nPadsAlongWire();
	     
	    }
	  delete padSimParBank;
	}
      
      //  application->commit();
     
    }
      
  else
    {
      application->abort();
      cerr << "padDetectorGeo::FetchFromSimDatabase() ERROR: Transaction aborted." << endl;
      return False;
    }
      
  return True;
  cout <<"this is the test of simpadgeo object\n";
         
}

// Fetch the geometry information from the default file padGeometry.txt
PHBoolean
padDetectorGeo::FetchFromFile()
{
  return FetchFromFile("padGeometry.txt");
}

// Fetch the geometry information from an ASCII file
PHBoolean
padDetectorGeo::FetchFromFile(const char* filename)
{
  // Set the logical database variable
  fromDatabase = False;
  int i, iarm, isect;
  short ipc;     // pad chamber number
  int ipoint;    // PHPanel point number (0-3)
  short nSectPc1, nSectPc2, nSectPc3;
  PHPoint panelPoint[4];
  double x, y, z;  // PHPanel coordinates
  // Open the file
  ifstream fin(filename);
  if (!fin)
    {
      cout << "padDetectorGeo::FetchFromFile-F:  Cannot open file.\n";
      return False;
    }
  // Start reading the pad chamber sector information.
  // The first line contains 3 numbers specifying the number
  // of geometry objects for each pad chamber
  // Then, each object is defined by the following format
  // Format is (pc number) (arm number) (sector number)
  //                              (point, [0-3]) (x) (y) (z)
  // Fetch the number of active sectors for each pad chamber
  fin >> nSectPc1 >> nSectPc2 >> nSectPc3;
  // Read in the sectors for PC1 and store the data members
  for (i = 0; i < nSectPc1; i++)
    {
      // Read in the indexing for this sector
      fin >> ipc >> iarm >> isect;
      if ((iarm < 2) && (isect < PC1MAXSECTPERARM))
        {
          pc1Active[iarm][isect] = 1;
        }
      else
        {
          cout << "padDetectorGeo::FetchFromFile-F:  PC1 data format error.\n";
          return False;
        }
      // Read in the coordinates
      for (ipoint = 0; ipoint < 4; ipoint++)
        {
          fin >> x >> y >> z;
          panelPoint[ipoint].setX(x);
          panelPoint[ipoint].setY(y);
          panelPoint[ipoint].setZ(z);
        }
      // Store the PHPanel object
      PHPanel pc1Panel(panelPoint[0], panelPoint[1], panelPoint[2]);
      pc1Sectors[iarm][isect] = pc1Panel;
    }
  // Read in the sectors for PC2 and store the data members
  for (i = 0; i < nSectPc2; i++)
    {
      // Read in the indexing for this sector
      fin >> ipc >> iarm >> isect;
      if ((iarm < 2) && (isect < CGLMAXSECTPERARM))
        {
          pc2Active[iarm][isect] = 1;
        }
      else
        {
          cout << "padDetectorGeo::FetchFromFile-F:  PC2 data format error.\n";
          return False;
        }
      // Read in the coordinates
      for (ipoint = 0; ipoint < 4; ipoint++)
        {
          fin >> x >> y >> z;
          panelPoint[ipoint].setX(x);
          panelPoint[ipoint].setY(y);
          panelPoint[ipoint].setZ(z);
        }
      // Store the PHPanel object
      PHPanel* pc2Panel = new PHPanel(panelPoint[0], panelPoint[1], panelPoint[2]);
      pc2Sectors[iarm][isect] = *pc2Panel;
      delete pc2Panel;
    }
  // Read in the sectors for PC3 and store the data members
  for (i = 0; i < nSectPc3; i++)
    {
      // Read in the indexing for this sector
      fin >> ipc >> iarm >> isect;
      if ((iarm < 2) && (isect < CGLMAXSECTPERARM))
        {
          pc3Active[iarm][isect] = 1;
        }
      else
        {
          cout << "padDetectorGeo::FetchFromFile-F:  PC3 data format error.\n";
          return False;
        }
      // Read in the coordinates
      for (ipoint = 0; ipoint < 4; ipoint++)
        {
          fin >> x >> y >> z;
          panelPoint[ipoint].setX(x);
          panelPoint[ipoint].setY(y);
          panelPoint[ipoint].setZ(z);
        }
      // Store the PHPanel object
      PHPanel* pc3Panel = new PHPanel(panelPoint[0], panelPoint[1], panelPoint[2]);
      pc3Sectors[iarm][isect] = *pc3Panel;
      delete pc3Panel;
    }
  // Read in the other data members
  fin >> xOffset[0] >> xOffset[1] >> xOffset[2];
  fin >> zOffset[0] >> zOffset[1] >> zOffset[2];
  fin >> gasAtten[0] >> gasAtten[1] >> gasAtten[2];
  fin >> anodeSpacing[0] >> anodeSpacing[1] >> anodeSpacing[2];
  fin >> pixelLength[0] >> pixelLength[1] >> pixelLength[2];
  fin >> sideWidth[0] >> sideWidth[1] >> sideWidth[2];
  fin >> centerWidth[0] >> centerWidth[1] >> centerWidth[2];
  fin >> pixelSpacing[0] >> pixelSpacing[1] >> pixelSpacing[2];
  fin >> cellSpacing[0] >> cellSpacing[1] >> cellSpacing[2];
  fin >> z0Gap[0] >> z0Gap[1] >> z0Gap[2];
  fin >> nWiresPerSect[0] >> nWiresPerSect[1] >> nWiresPerSect[2];
  fin >> nPadsAcrossWire[0] >> nPadsAcrossWire[1] >> nPadsAcrossWire[2];
  fin >> nPadsAlongWire[0] >> nPadsAlongWire[1] >> nPadsAlongWire[2];
  // Close the file
  fin.close();
  return True;
}

// Fetch the geometry information from an ASCII file
PHBoolean
padDetectorGeo::FetchFromFileDBFormat()
{
  return FetchFromFileDBFormat("padGeometry.txt");
}

// Fetch the geometry information from an ASCII file
PHBoolean
padDetectorGeo::FetchFromFileDBFormat(const char* filename)
{
  // Set the logical database variable
  // (yes, this seems wrong: but we are putting in DB style info, so..)
  fromDatabase = True;
  int i, iarm, isect;
  int ipoint;    // PHPanel point number (0-3)
  short ipc;     // pad chamber number
  double x, y, z;  // PHPanel coordinates
  PHPoint northUpPoint, southUpPoint, northDownPoint, southDownPoint;
  PHPoint panelPoint[8];
  // Open the file
  ifstream fin(filename);
  if (!fin)
    {
      cout << "padDetectorGeo::FetchFromFileDBFormat-F:  Cannot open file.\n";
      return False;
    }
  // Format:
  // (pc number) (arm number) (sector number) (point, [0-3]) (x) (y) (z)
  // Fetch the number of active sectors for each pad chamber
  fin >> nChamPc1 >> nChamPc2 >> nChamPc3;
  // Read in the sectors for PC1 and store the data members
  for (i = 0; i < nChamPc1; i++)
    {
      // Read in the indexing for this sector
      fin >> ipc >> iarm >> isect;
      if ((iarm < 2) && (isect < PC1MAXSECTPERARM))
        {
          pc1Active[iarm][isect] = 1;
        }
      else
        {
          cout << "padDetectorGeo::FetchFromFileDBFormat-F:  PC1 data format error.\n";
          return False;
        }
      // Read in the coordinates
      for (ipoint = 0; ipoint < 4; ipoint++)
        {
          fin >> x >> y >> z;
          panelPoint[ipoint].setX(x);
          panelPoint[ipoint].setY(y);
          panelPoint[ipoint].setZ(z);
        }
      // Store the PHPanel object
      PHPanel pc1Panel(panelPoint[0], panelPoint[1], panelPoint[2]);
      pc1Sectors[iarm][isect] = pc1Panel;
    }
  // Read in the sectors for PC2 and store the data members
  for (i = 0; i < nChamPc2; i++)
    {
      // Read in the indexing for this sector
      fin >> ipc >> iarm >> isect;
      if ((iarm < 2) && (isect < CGLMAXSECTPERARM))
        {
          pc2Active[iarm][isect] = 1;
        }
      else
        {
          cout << "padDetectorGeo::FetchFromFileDBFormat-F:  PC2 data format error.\n";
          return False;
        }
      // Read in the coordinates
      for (ipoint = 0; ipoint < 8; ipoint++)
        {
          fin >> x >> y >> z;
          panelPoint[ipoint].setX(x);
          panelPoint[ipoint].setY(y);
          panelPoint[ipoint].setZ(z);
        }
      // Store the sector and chamber objects
      PHPanel* pc2Sector = new PHPanel(panelPoint[0], panelPoint[1], panelPoint[6]);
      pc2Sectors[iarm][isect] = *pc2Sector;
      delete pc2Sector;
      PHPanel* pc2Chamber0 = new PHPanel(panelPoint[0], panelPoint[1], panelPoint[2]);
      pc2Chambers[iarm][isect][0] = *pc2Chamber0;
      delete pc2Chamber0;
      PHPanel* pc2Chamber1 = new PHPanel(panelPoint[4], panelPoint[5], panelPoint[6]);
      pc2Chambers[iarm][isect][1] = *pc2Chamber1;
      delete pc2Chamber1;
    }
  // Read in the sectors for PC3 and store the data members
  for (i = 0; i < nChamPc3; i++)
    {
      // Read in the indexing for this sector
      fin >> ipc >> iarm >> isect;
      if ((iarm < 2) && (isect < CGLMAXSECTPERARM))
        {
          pc3Active[iarm][isect] = 1;
        }
      else
        {
          cout << "padDetectorGeo::FetchFromFileDBFormat-F:  PC3 data format error.\n";
          return False;
        }
      // Read in the coordinates
      for (ipoint = 0; ipoint < 8; ipoint++)
        {
          fin >> x >> y >> z;
          panelPoint[ipoint].setX(x);
          panelPoint[ipoint].setY(y);
          panelPoint[ipoint].setZ(z);
        }
      // Store the sector and chamber objects
      PHPanel* pc3Sector = new PHPanel(panelPoint[0], panelPoint[1], panelPoint[6]);
      pc3Sectors[iarm][isect] = *pc3Sector;
      delete pc3Sector;
      PHPanel* pc3Chamber0 = new PHPanel(panelPoint[0], panelPoint[1], panelPoint[2]);
      pc3Chambers[iarm][isect][0] = *pc3Chamber0;
      delete pc3Chamber0;
      PHPanel* pc3Chamber1 = new PHPanel(panelPoint[4], panelPoint[5], panelPoint[6]);
      pc3Chambers[iarm][isect][1] = *pc3Chamber1;
      delete pc3Chamber1;
    }
  // Read in the other data members
  fin >> xOffset[0] >> xOffset[1] >> xOffset[2];
  fin >> zOffset[0] >> zOffset[1] >> zOffset[2];
  fin >> gasAtten[0] >> gasAtten[1] >> gasAtten[2];
  fin >> anodeSpacing[0] >> anodeSpacing[1] >> anodeSpacing[2];
  fin >> pixelLength[0] >> pixelLength[1] >> pixelLength[2];
  fin >> sideWidth[0] >> sideWidth[1] >> sideWidth[2];
  fin >> centerWidth[0] >> centerWidth[1] >> centerWidth[2];
  fin >> pixelSpacing[0] >> pixelSpacing[1] >> pixelSpacing[2];
  fin >> cellSpacing[0] >> cellSpacing[1] >> cellSpacing[2];
  fin >> z0Gap[0] >> z0Gap[1] >> z0Gap[2];
  fin >> nWiresPerSect[0] >> nWiresPerSect[1] >> nWiresPerSect[2];
  fin >> nPadsAcrossWire[0] >> nPadsAcrossWire[1] >> nPadsAcrossWire[2];
  fin >> nPadsAlongWire[0] >> nPadsAlongWire[1] >> nPadsAlongWire[2];
  // Close the file
  fin.close();
  return True;
}

// Put the geometry information into the dPadGeom table
void
padDetectorGeo::Put_dPadGeom(PHCompositeNode* topNode)
{
  int i;
  // Fetch the pointer to dPadGeom from the topNode
  typedef PHIODataNode<dPadGeomWrapper> dPadGeomNode;
  PHNodeIterator ini(topNode);
  dPadGeomNode* geomNode = static_cast<dPadGeomNode*>(ini.findFirst("PHIODataNode", "dPadGeom"));
  dPadGeomWrapper* padGeom = geomNode->getData();
  // Put in the pad-chamber-specific quantities
  // All other quantities are determined by fetch or build methods
  for (i = 0; i < 3; i++)
    {
      xOffset[i] = padGeom->get_pdxoff(i, 0);
      zOffset[i] = padGeom->get_pdzoff(i, 0);
      gasAtten[i] = padGeom->get_pdgas(i, 0);
      anodeSpacing[i] = padGeom->get_aasep(i, 0);
      pixelLength[i] = padGeom->get_pxlen(i, 0);
      sideWidth[i] = padGeom->get_wside(i, 0);
      centerWidth[i] = padGeom->get_wcent(i, 0);
      pixelSpacing[i] = padGeom->get_pxsep(i, 0);
      cellSpacing[i] = padGeom->get_clsep(i, 0);
      z0Gap[i] = padGeom->get_zgap(i, 0);
      nWiresPerSect[i] = padGeom->get_npdwr(i, 0);
      nPadsAcrossWire[i] = padGeom->get_npdx(i, 0);
      nPadsAlongWire[i] = padGeom->get_npdz(i, 0);
    }
  pc1Radius = padGeom->get_inradius(0, 0);
  pc2Radius = padGeom->get_inradius(1, 0);
  pc3Radius = padGeom->get_inradius(2, 0);
}

// Put the geometry information into the database
PHBoolean
padDetectorGeo::PutIntoGeoChamDatabase()
{
  // Open database in update mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (application->startUpdate())
    {
      // Put corresponding bank
      setCalibName("geom.pad.geocham");
      setBankNumber(4104);
      setBankID(BankNumber);
      const char *description = "PC geometry";
      // Create the bank object
      PdbCalBank *padBank = bankManager->createBank("PdbPadGeoChamBank", 
						    BankID,
						    description, 
						    TUpdateStart,
						    TUpdateStop, 
						    "geom.pad.geocham");
      // Allocate space for 8 chambers per PC
      int numberOfChambersPerPC = 8;
      int numberOfChamberFillings = 6 * numberOfChambersPerPC;
      padBank->setLength(numberOfChamberFillings);

      // Create the PdbPadGeoCham object
      PdbPadGeoCham *padgeochamobj;
      int chamber = 0;
      PHPoint northUpPoint, southUpPoint, northDownPoint, southDownPoint;
      // Store the corner points for PC1 in the PdbPadGeoCham object
      for (int arm = 0; arm < 2; arm++)
        {
          for (int sector = 0; sector < PC1MAXSECTPERARM; sector++)
            {
              // Fetch the points
              southDownPoint = pc1Sectors[arm][sector].getPoint(0);
              southUpPoint = pc1Sectors[arm][sector].getPoint(1);
              northDownPoint = pc1Sectors[arm][sector].getPoint(2);
              northUpPoint = pc1Sectors[arm][sector].getPoint(3);
              cout << "#############" << endl;
              southDownPoint.print();
              southUpPoint.print();
              northDownPoint.print();
              northUpPoint.print();
              cout << "#############" << endl;
              // Get the PdbPadGeoCham object
              padgeochamobj = (PdbPadGeoCham*) & (padBank->getEntry(chamber));
              // Store the points in the PdbPadGeoCham object
              padgeochamobj->setSouthDownPoint(southDownPoint);
              padgeochamobj->setSouthUpPoint(southUpPoint);
              padgeochamobj->setNorthDownPoint(northDownPoint);
              padgeochamobj->setNorthUpPoint(northUpPoint);
              padgeochamobj->setPC(0);
              padgeochamobj->setArm(arm);
              padgeochamobj->setSector(sector);
              padgeochamobj->setSide(0);

              chamber++;
            }
        }
      // Store the corner points for PC2 in the PdbPadGeoCham object
      for (int arm = 0; arm < 2; arm++)
        {
          for (int sector = 0; sector < CGLMAXSECTPERARM; sector++)
            {
              for (int side = 0; side < 2; side++)
                {
                  // Fetch the points
                  southDownPoint = pc2Chambers[arm][sector][side].getPoint(0);
                  southUpPoint = pc2Chambers[arm][sector][side].getPoint(1);
                  northDownPoint = pc2Chambers[arm][sector][side].getPoint(2);
                  northUpPoint = pc2Chambers[arm][sector][side].getPoint(3);
                  // Get the PdbPadGeoCham object
                  padgeochamobj = (PdbPadGeoCham*) & (padBank->getEntry(chamber));
                  // Store the data in the PdbPadGeoCham object
                  padgeochamobj->setSouthDownPoint(southDownPoint);
                  padgeochamobj->setSouthUpPoint(southUpPoint);
                  padgeochamobj->setNorthDownPoint(northDownPoint);
                  padgeochamobj->setNorthUpPoint(northUpPoint);
                  padgeochamobj->setPC(1);
                  padgeochamobj->setArm(arm);
                  padgeochamobj->setSector(sector);
                  padgeochamobj->setSide(side);
                  chamber++;
                }
            }
        }
      // Store the corner points for PC3 in the PdbPadGeoCham object
      for (int arm = 0; arm < 2; arm++)
        {
          for (int sector = 0; sector < CGLMAXSECTPERARM; sector++)
            {
              for (int side = 0; side < 2; side++)
                {
                  // Fetch the points
                  southDownPoint = pc3Chambers[arm][sector][side].getPoint(0);
                  southUpPoint = pc3Chambers[arm][sector][side].getPoint(1);
                  northDownPoint = pc3Chambers[arm][sector][side].getPoint(2);
                  northUpPoint = pc3Chambers[arm][sector][side].getPoint(3);
                  // Get the PdbPadGeoCham object
                  padgeochamobj = (PdbPadGeoCham*) & (padBank->getEntry(chamber));
                  // Store the data in the PdbPadGeoCham object
                  padgeochamobj->setSouthDownPoint(southDownPoint);
                  padgeochamobj->setSouthUpPoint(southUpPoint);
                  padgeochamobj->setNorthDownPoint(northDownPoint);
                  padgeochamobj->setNorthUpPoint(northUpPoint);
                  padgeochamobj->setPC(2);
                  padgeochamobj->setArm(arm);
                  padgeochamobj->setSector(sector);
                  padgeochamobj->setSide(side);
                  chamber++;
                }
            }
        }
      application->commit();
    }
  else
    {
      application->abort();
      cerr << "padDetectorGeo::PutIntoGeoChamDatabase() ERROR: Transaction aborted. Failed to start application." << endl;
      return False;
    }
  return True;
}

// Put the geometry information into the database
PHBoolean
padDetectorGeo::PutIntoGeoParDatabase()
{
  // Open database in update mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (application->startUpdate())
    {
      // Pad geometry parameters
      setCalibName("geom.pad.geopar");
      setBankNumber(4105);
      setBankID(BankNumber);
      const char *description = "PC geometry parameters";
      // Create the bank object
      PdbCalBank *padParBank = bankManager->createBank("PdbPadGeoParBank", BankID,
                               description, TUpdateStart, TUpdateStop, "geom.pad.geopar");
      // Allocate space for 3 parameter fillings
      int numberOfParameterFillings = 3;
      padParBank->setLength(numberOfParameterFillings);
      // Create the PdbPadGeoPar object
      PdbPadGeoPar *padgeoparobj;
      // Store the parameters in the PdbPadGeoPar object
      // How many sectors are active?
      int nActiveSectors[3], nSectPerArm[3];
      nActiveSectors[0] = 0;
      nActiveSectors[1] = 0;
      nActiveSectors[2] = 0;
      nSectPerArm[0] = nChamPc1;
      nSectPerArm[1] = nChamPc2;
      nSectPerArm[2] = nChamPc3;
      for (int arm = 0; arm < 2; arm++)
        {
          for (int sector = 0; sector < PC1MAXSECTPERARM; sector++)
            {
              nActiveSectors[0] += pc1Active[arm][sector];
            }
          for (int sector = 0; sector < CGLMAXSECTPERARM; sector++)
            {
              nActiveSectors[1] += pc2Active[arm][sector];
              nActiveSectors[2] += pc3Active[arm][sector];
            }
        }
      // Store the points in the PdbPadGeoPar object
      double radius;
      for (int pc = 0; pc < 3; pc++)
        {
          if (pc == 0)
            {
	      radius = get_pc1Radius();
	    }
          else if (pc == 1)
            {
	      radius = get_pc2Radius();
	    }
          else
            {
	      radius = get_pc3Radius();
	    }
          padDetectorGeo::get_PhiTop(PhiTop);
          padDetectorGeo::get_PhiBottom(PhiBottom);
          // Get the PdbPadGeoPar object
          padgeoparobj = (PdbPadGeoPar*) & (padParBank->getEntry(pc));
          padgeoparobj->set_PC(pc);
          padgeoparobj->set_gasAtten(gasAtten[pc]);
          padgeoparobj->set_anodeSpacing(anodeSpacing[pc]);
          padgeoparobj->set_pixelLength(pixelLength[pc]);
          padgeoparobj->set_sidePixelWidth(sideWidth[pc]);
          padgeoparobj->set_centerPixelWidth(centerWidth[pc]);
          padgeoparobj->set_pixelSpacing(pixelSpacing[pc]);
          padgeoparobj->set_cellSpacing(cellSpacing[pc]);
          padgeoparobj->set_nActiveSectors(nActiveSectors[pc]);
          padgeoparobj->set_nSectPerArm(nSectPerArm[pc]);
          padgeoparobj->set_nWiresPerSect(nWiresPerSect[pc]);
          padgeoparobj->set_nPadsAcrossWire(nPadsAcrossWire[pc]);
          padgeoparobj->set_nPadsAlongWire(nPadsAlongWire[pc]);
          padgeoparobj->set_z0Gap(z0Gap[pc]);
          padgeoparobj->set_xOffset(xOffset[pc]);
          padgeoparobj->set_zOffset(zOffset[pc]);
          padgeoparobj->set_pcRadius(radius);
          padgeoparobj->set_phiBottomEast(PhiBottom[0]);
          padgeoparobj->set_phiTopEast(PhiTop[0]);
          padgeoparobj->set_phiBottomWest(PhiBottom[1]);
          padgeoparobj->set_phiTopWest(PhiTop[1]);
        }
      application->commit();
    }
  else
    {
      application->abort();
      cerr << "padDetectorGeo::PutIntoGeoParDatabase() ERROR: Transaction aborted. Failed to start application." << endl;
      return False;
    }
  return True;
}
///////////////////////////////////////////////////////////////////////////
PHBoolean
padDetectorGeo::PutIntoSimGeoChamDatabase()
{
  short nSectPc1, nSectPc2, nSectPc3, nSectors;
  double x0, y0, z0;  // PHPanel coordinates
  double x1, y1, z1;  // PHPanel coordinates
  double x2, y2, z2;  // PHPanel coordinates
  double x3, y3, z3;  // PHPanel coordinates
  PHPoint panelPoint[4];
  
  // Open database in update mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (application->startUpdate())
    {
      // Put corresponding bank
      setCalibName("geom.pad.simgeocham");
      setBankNumber(4110);
      setBankID(BankNumber);
      const char *description = "PC simulation geometry";
      // Create the bank object
      PdbCalBank *padBank = bankManager->createBank("PdbPadSimGeoChamV2Bank", 
						    BankID,
						    description, 
						    TUpdateStart,
						    TUpdateStop, 
						    "geom.pad.simgeocham");
      // Allocate space for PC Sectors
	nSectPc1 = 2 * PC1MAXSECTPERARM;
        nSectPc2 = nSectPc3 = 2 * CGLMAXSECTPERARM;
        nSectors = nSectPc1 + nSectPc2 + nSectPc3;
        padBank->setLength(nSectors);

#ifdef DEBUG
      if (Debug == 1)
        {
	  padBank->print();
	}
#endif
// Create the PdbPadSimGeoChamV2 object
   PdbPadSimGeoChamV2 *padsimgeochamobj;
   int chamber = 0;
 
//For pc1
  for (int arm = 0; arm < 2; arm++)
    {//pc1
          for (int sector = 0; sector < PC1MAXSECTPERARM; sector++)
            {
		       panelPoint[0] = pc1Sectors[arm][sector].getPoint(0);
		        x0 = panelPoint[0].getX();
		        y0 = panelPoint[0].getY();
		        z0 = panelPoint[0].getZ();
		       panelPoint[1] = pc1Sectors[arm][sector].getPoint(1);
		        x1 = panelPoint[1].getX();
		        y1 = panelPoint[1].getY();
		        z1 = panelPoint[1].getZ();
		       panelPoint[2] = pc1Sectors[arm][sector].getPoint(2);
		        x2 = panelPoint[2].getX();
		        y2 = panelPoint[2].getY();
		        z2 = panelPoint[2].getZ();
		       panelPoint[3] = pc1Sectors[arm][sector].getPoint(3);
		        x3 = panelPoint[3].getX();
		        y3 = panelPoint[3].getY();
		        z3 = panelPoint[3].getZ();
		    
                  padsimgeochamobj = (PdbPadSimGeoChamV2*) & (padBank->getEntry(chamber));
		  padsimgeochamobj->set_pc(0);
		  padsimgeochamobj->set_arm(arm);
		  padsimgeochamobj->set_sect(sector);
		  padsimgeochamobj->set_point1(0,x0);
		  padsimgeochamobj->set_point1(1,y0);
		  padsimgeochamobj->set_point1(2,z0);
		  padsimgeochamobj->set_point2(0,x1);
		  padsimgeochamobj->set_point2(1,y1);
		  padsimgeochamobj->set_point2(2,z1);
		  padsimgeochamobj->set_point3(0,x2);
		  padsimgeochamobj->set_point3(1,y2);
		  padsimgeochamobj->set_point3(2,z2);
		  padsimgeochamobj->set_point4(0,x3);
		  padsimgeochamobj->set_point4(1,y3);
		  padsimgeochamobj->set_point4(2,z3);
                  chamber++;
		  padsimgeochamobj->print();

            }
        
   }

//pc2
  for (int arm = 0; arm < 2; arm++)
  {
          for (int sector = 0; sector < CGLMAXSECTPERARM; sector++)
            {
		       panelPoint[0] = pc2Sectors[arm][sector].getPoint(0);
		        x0 = panelPoint[0].getX();
		        y0 = panelPoint[0].getY();
		        z0 = panelPoint[0].getZ();
		       panelPoint[1] = pc2Sectors[arm][sector].getPoint(1);
		        x1 = panelPoint[1].getX();
		        y1 = panelPoint[1].getY();
		        z1 = panelPoint[1].getZ();
		       panelPoint[2] = pc2Sectors[arm][sector].getPoint(2);
		        x2 = panelPoint[2].getX();
		        y2 = panelPoint[2].getY();
		        z2 = panelPoint[2].getZ();
		       panelPoint[3] = pc2Sectors[arm][sector].getPoint(3);
		        x3 = panelPoint[3].getX();
		        y3 = panelPoint[3].getY();
		        z3 = panelPoint[3].getZ();
		    
                  padsimgeochamobj = (PdbPadSimGeoChamV2*) & (padBank->getEntry(chamber));
		  padsimgeochamobj->set_pc(1);
		  padsimgeochamobj->set_arm(arm);
		  padsimgeochamobj->set_sect(sector);
		  padsimgeochamobj->set_point1(0,x0);
		  padsimgeochamobj->set_point1(1,y0);
		  padsimgeochamobj->set_point1(2,z0);
		  padsimgeochamobj->set_point2(0,x1);
		  padsimgeochamobj->set_point2(1,y1);
		  padsimgeochamobj->set_point2(2,z1);
		  padsimgeochamobj->set_point3(0,x2);
		  padsimgeochamobj->set_point3(1,y2);
		  padsimgeochamobj->set_point3(2,z2);
		  padsimgeochamobj->set_point4(0,x3);
		  padsimgeochamobj->set_point4(1,y3);
		  padsimgeochamobj->set_point4(2,z3);
                  chamber++;
		  padsimgeochamobj->print();
            }
        
   }
	  //pc3

  for (int arm = 0; arm < 2; arm++)
    {//pc3
          for (int sector = 0; sector < CGLMAXSECTPERARM; sector++)
            {
		       panelPoint[0] = pc3Sectors[arm][sector].getPoint(0);
		        x0 = panelPoint[0].getX();
		        y0 = panelPoint[0].getY();
		        z0 = panelPoint[0].getZ();
		       panelPoint[1] = pc3Sectors[arm][sector].getPoint(1);
		        x1 = panelPoint[1].getX();
		        y1 = panelPoint[1].getY();
		        z1 = panelPoint[1].getZ();
		       panelPoint[2] = pc3Sectors[arm][sector].getPoint(2);
		        x2 = panelPoint[2].getX();
		        y2 = panelPoint[2].getY();
		        z2 = panelPoint[2].getZ();
		       panelPoint[3] = pc3Sectors[arm][sector].getPoint(3);
		        x3 = panelPoint[3].getX();
		        y3 = panelPoint[3].getY();
		        z3 = panelPoint[3].getZ();
		    
                  padsimgeochamobj = (PdbPadSimGeoChamV2*) & (padBank->getEntry(chamber));
		  padsimgeochamobj->set_pc(2);
		  padsimgeochamobj->set_arm(arm);
		  padsimgeochamobj->set_sect(sector);
		  padsimgeochamobj->set_point1(0,x0);
		  padsimgeochamobj->set_point1(1,y0);
		  padsimgeochamobj->set_point1(2,z0);
		  padsimgeochamobj->set_point2(0,x1);
		  padsimgeochamobj->set_point2(1,y1);
		  padsimgeochamobj->set_point2(2,z1);
		  padsimgeochamobj->set_point3(0,x2);
		  padsimgeochamobj->set_point3(1,y2);
		  padsimgeochamobj->set_point3(2,z2);
		  padsimgeochamobj->set_point4(0,x3);
		  padsimgeochamobj->set_point4(1,y3);
		  padsimgeochamobj->set_point4(2,z3);
                  chamber++;
		  padsimgeochamobj->print();
            }
        
   }

 //printing the padBank
  
	   padBank->print();
	   for(int k=0;k<32;k++){
	   padBank->printEntry(k);
	   }
	   application->commit(padBank);
    }
  else
    {
      application->abort();
      cerr << "padDetectorGeo::PutIntoSimGeoDatabase() ERROR: Transaction aborted. Failed      to start application." << endl;
      return False;
    }
  return True;
}
PHBoolean
padDetectorGeo::PutIntoSimGeoParDatabase()
{

  // Open database in update mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (application->startUpdate())
    {
      // set corresponding bank
      setCalibName("geom.pad.simgeopar");
      setBankNumber(4111);
      setBankID(BankNumber);
      const char *description = "PC simulation geometry parameters";
      // Create the bank object
      PdbCalBank *padBank = bankManager->createBank("PdbPadSimGeoParV1Bank", 
						    BankID,
						    description, 
						    TUpdateStart,
						    TUpdateStop, 
						    "geom.pad.simgeopar");
      //Create a sim geometry parameter object
      int numberofchamber = 3;
      padBank->setLength(numberofchamber);

      PdbPadSimGeoParV1 *padsimgeoparobj;
   
      for (int i=0; i<3; i++)
	{
	  padsimgeoparobj = (PdbPadSimGeoParV1*) & padBank->getEntry(i);
	
	  //set parameters
	   
	  padsimgeoparobj->set_z0Gap(z0Gap[i]);
	  padsimgeoparobj->set_xOffset(xOffset[i]);
	  padsimgeoparobj->set_zOffset(zOffset[i]);
	  padsimgeoparobj->set_gasAtten(gasAtten[i]);
	  padsimgeoparobj->set_anodeSpacing(anodeSpacing[i]);
	  padsimgeoparobj->set_pixelLength(pixelLength[i]);
	  padsimgeoparobj->set_sideWidth(sideWidth[i]);
	  padsimgeoparobj->set_centerWidth(centerWidth[i]);
	  padsimgeoparobj->set_pixelSpacing(pixelSpacing[i]);
	  padsimgeoparobj->set_cellSpacing(cellSpacing[i]);
	  padsimgeoparobj->set_nWiresPerSect(nWiresPerSect[i]);
	  padsimgeoparobj->set_nPadsAcrossWire(nPadsAcrossWire[i]);
	  padsimgeoparobj->set_nPadsAlongWire(nPadsAlongWire[i]);
	  
    }
      application->commit();
    }
  else
    {
      application->abort();
      cerr << "padDetectorGeo::PutIntoSimGeoParDatabase() ERROR: Transaction aborted. Failed      to start application." << endl;
      return False;
    }
  return True;
}     

////////////////////////////////////////////////////////////////////////////
// Put the geometry information into the default file padGeometry.txt
PHBoolean
padDetectorGeo::PutIntoFile()
{
  return PutIntoFile("padGeometry.txt");
}

// Put the geometry information into an ASCII file
PHBoolean
padDetectorGeo::PutIntoFile(const char* filename)
{
  int i, iarm, isect;
  int ipoint; // PHPanel point number (0-3)
  short ipc;  // pad chamber number
  short nSectPc1, nSectPc2, nSectPc3;
  double x, y, z;  // PHPanel coordinates
  PHPoint panelPoint;
  // Open the file
  ofstream fout(filename);
  if (!fout)
    {
      cout << "padDetectorGeo::PutIntoFile-W:  Cannot open file.\n";
      return False;
    }
  // Start writing the pad chamber sector information.
  // The first line contains 3 numbers specifying the number
  // of geometry objects for each pad chamber
  // Then, each object is defined by the following format
  // Format is (pc number) (arm number) (sector number)
  //                              (point, [0-3]) (x) (y) (z)
  // Count the number of active sectors for each pad chamber
  nSectPc1 = 0;
  nSectPc2 = 0;
  nSectPc3 = 0;
  for (iarm = 0; iarm < 2; iarm++)
    {
      for (i = 0; i < PC1MAXSECTPERARM; i++)
        {
          if (pc1Active[iarm][i] == 1)
            {
	      nSectPc1++;
	    }
        }
    }
  for (iarm = 0; iarm < 2; iarm++)
    {
      for (i = 0; i < CGLMAXSECTPERARM; i++)
        {
          if (pc2Active[iarm][i] == 1)
            {
	      nSectPc2++;
	    }
          if (pc3Active[iarm][i] == 1)
            {
	      nSectPc3++;
	    }
        }
    }
  fout << nSectPc1 << " " << nSectPc2 << " " << nSectPc3 << "\n";
  // Write out the sectors
  ipc = 0;
  for (iarm = 0; iarm < 2; iarm++)
    {
      isect = 0;
      for (i = 0; i < PC1MAXSECTPERARM; i++)
        {
          if (pc1Active[iarm][i] == 1)
            {
              fout << ipc << " " << iarm << " " << isect << "\n";
              for (ipoint = 0; ipoint < 4; ipoint++)
                {
                  panelPoint = pc1Sectors[iarm][i].getPoint(ipoint);
                  x = panelPoint.getX();
                  y = panelPoint.getY();
                  z = panelPoint.getZ();
                  fout << "  " << x << " " << y << " " << z << "\n";
                }
              isect++;
            }
        }
    }
  ipc = 1;
  for (iarm = 0; iarm < 2; iarm++)
    {
      isect = 0;
      for (i = 0; i < CGLMAXSECTPERARM; i++)
        {
          if (pc2Active[iarm][i] == 1)
            {
              fout << ipc << " " << iarm << " " << isect << "\n";
              for (ipoint = 0; ipoint < 4; ipoint++)
                {
                  panelPoint = pc2Sectors[iarm][i].getPoint(ipoint);
                  x = panelPoint.getX();
                  y = panelPoint.getY();
                  z = panelPoint.getZ();
                  fout << "  " << x << " " << y << " " << z << "\n";
                }
              isect++;
            }
        }
    }
  ipc = 2;
  for (iarm = 0; iarm < 2; iarm++)
    {
      isect = 0;
      for (i = 0; i < CGLMAXSECTPERARM; i++)
        {
          if (pc3Active[iarm][i] == 1)
            {
              fout << ipc << " " << iarm << " " << isect << "\n";
              for (ipoint = 0; ipoint < 4; ipoint++)
                {
                  panelPoint = pc3Sectors[iarm][i].getPoint(ipoint);
                  x = panelPoint.getX();
                  y = panelPoint.getY();
                  z = panelPoint.getZ();
                  fout << "  " << x << " " << y << " " << z << "\n";
                }
              isect++;
            }
        }
    }
  // Write out the other data members
  fout << xOffset[0] << " " << xOffset[1] << " " <<
  xOffset[2] << "\n";
  fout << zOffset[0] << " " << zOffset[1] << " " <<
  zOffset[2] << "\n";
  fout << gasAtten[0] << " " << gasAtten[1] << " " <<
  gasAtten[2] << "\n";
  fout << anodeSpacing[0] << " " << anodeSpacing[1] << " " <<
  anodeSpacing[2] << "\n";
  fout << pixelLength[0] << " " << pixelLength[1] << " " <<
  pixelLength[2] << "\n";
  fout << sideWidth[0] << " " << sideWidth[1] << " " <<
  sideWidth[2] << "\n";
  fout << centerWidth[0] << " " << centerWidth[1] << " " <<
  centerWidth[2] << "\n";
  fout << pixelSpacing[0] << " " << pixelSpacing[1] << " " <<
  pixelSpacing[2] << "\n";
  fout << cellSpacing[0] << " " << cellSpacing[1] << " " <<
  cellSpacing[2] << "\n";
  fout << z0Gap[0] << " " << z0Gap[1] << " " <<
  z0Gap[2] << "\n";
  fout << nWiresPerSect[0] << " " << nWiresPerSect[1] << " " <<
  nWiresPerSect[2] << "\n";
  fout << nPadsAcrossWire[0] << " " << nPadsAcrossWire[1] << " " <<
  nPadsAcrossWire[2] << "\n";
  fout << nPadsAlongWire[0] << " " << nPadsAlongWire[1] << " " <<
  nPadsAlongWire[2] << "\n";
  // Close the file
  fout.close();
  return True;
}

// Put the geometry information into an ASCII file
PHBoolean
padDetectorGeo::PutIntoFileDBFormat(const char* filename)
{
  int i, iarm, isect, iside;
  int ipoint; // PHPanel point number (0-3)
  short ipc;  // pad chamber number
  short nSectPc1, nSectPc2, nSectPc3;
  double x, y, z;  // PHPanel coordinates
  PHPoint panelPoint;
  // Open the file
  ofstream fout(filename);
  if (!fout)
    {
      cout << "padDetectorGeo::PutIntoFileDBFormat-W:  Cannot open file.\n";
      return False;
    }
  // Start writing the pad chamber sector information.
  // The first line contains 3 numbers specifying the number
  // of geometry objects for each pad chamber
  // Then, each object is defined by the following format
  // Format is (pc number) (arm number) (sector number)
  //                              (point, [0-3]) (x) (y) (z)
  // Count the number of active sectors for each pad chamber
  nSectPc1 = 0;
  nSectPc2 = 0;
  nSectPc3 = 0;
  for (iarm = 0; iarm < 2; iarm++)
    {
      for (i = 0; i < PC1MAXSECTPERARM; i++)
        {
          if (pc1Active[iarm][i] == 1)
            {
	      nSectPc1++;
	    }
        }
    }
  for (iarm = 0; iarm < 2; iarm++)
    {
      for (i = 0; i < CGLMAXSECTPERARM; i++)
        {
          if (pc2Active[iarm][i] == 1)
            {
	      nSectPc2++;
	    }
          if (pc3Active[iarm][i] == 1)
            {
	      nSectPc3++;
	    }
        }
    }
  fout << nSectPc1 << " " << nSectPc2 << " " << nSectPc3 << "\n";
  // Write out the sectors
  ipc = 0;
  for (iarm = 0; iarm < 2; iarm++)
    {
      isect = 0;
      for (i = 0; i < PC1MAXSECTPERARM; i++)
        {
          if (pc1Active[iarm][i] == 1)
            {
              fout << ipc << " " << iarm << " " << isect << "\n";
              for (ipoint = 0; ipoint < 4; ipoint++)
                {
                  panelPoint = pc1Sectors[iarm][i].getPoint(ipoint);
                  x = panelPoint.getX();
                  y = panelPoint.getY();
                  z = panelPoint.getZ();
                  fout << "  " << x << " " << y << " " << z << "\n";
                }
              isect++;
            }
        }
    }
  ipc = 1;
  for (iarm = 0; iarm < 2; iarm++)
    {
      isect = 0;
      for (i = 0; i < CGLMAXSECTPERARM; i++)
        {
          if (pc2Active[iarm][i] == 1)
            {
              fout << ipc << " " << iarm << " " << isect << "\n";
	      for (iside = 0; iside < 2; iside++)
		{
		  for (ipoint = 0; ipoint < 4; ipoint++)
		    {
		      panelPoint = pc2Chambers[iarm][i][iside].getPoint(ipoint);
		      x = panelPoint.getX();
		      y = panelPoint.getY();
		      z = panelPoint.getZ();
		      fout << "  " << x << " " << y << " " << z << "\n";
		    }
		}
              isect++;
            }
        }
    }
  ipc = 2;
  for (iarm = 0; iarm < 2; iarm++)
    {
      isect = 0;
      for (i = 0; i < CGLMAXSECTPERARM; i++)
        {
          if (pc3Active[iarm][i] == 1)
            {
              fout << ipc << " " << iarm << " " << isect << "\n";
	      for (iside = 0; iside < 2; iside++)
		{
		  for (ipoint = 0; ipoint < 4; ipoint++)
		    {
		      panelPoint = pc3Chambers[iarm][i][iside].getPoint(ipoint);
		      x = panelPoint.getX();
		      y = panelPoint.getY();
		      z = panelPoint.getZ();
		      fout << "  " << x << " " << y << " " << z << "\n";
		    }
		}
              isect++;
            }
        }
    }
  // Write out the other data members
  fout << xOffset[0] << " " << xOffset[1] << " " <<
  xOffset[2] << "\n";
  fout << zOffset[0] << " " << zOffset[1] << " " <<
  zOffset[2] << "\n";
  fout << gasAtten[0] << " " << gasAtten[1] << " " <<
  gasAtten[2] << "\n";
  fout << anodeSpacing[0] << " " << anodeSpacing[1] << " " <<
  anodeSpacing[2] << "\n";
  fout << pixelLength[0] << " " << pixelLength[1] << " " <<
  pixelLength[2] << "\n";
  fout << sideWidth[0] << " " << sideWidth[1] << " " <<
  sideWidth[2] << "\n";
  fout << centerWidth[0] << " " << centerWidth[1] << " " <<
  centerWidth[2] << "\n";
  fout << pixelSpacing[0] << " " << pixelSpacing[1] << " " <<
  pixelSpacing[2] << "\n";
  fout << cellSpacing[0] << " " << cellSpacing[1] << " " <<
  cellSpacing[2] << "\n";
  fout << z0Gap[0] << " " << z0Gap[1] << " " <<
  z0Gap[2] << "\n";
  fout << nWiresPerSect[0] << " " << nWiresPerSect[1] << " " <<
  nWiresPerSect[2] << "\n";
  fout << nPadsAcrossWire[0] << " " << nPadsAcrossWire[1] << " " <<
  nPadsAcrossWire[2] << "\n";
  fout << nPadsAlongWire[0] << " " << nPadsAlongWire[1] << " " <<
  nPadsAlongWire[2] << "\n";
  // Close the file
  fout.close();
  return True;
}

void
padDetectorGeo::set_pc1Active(short arm, short pc1in[])
{
  int i;
  for (i = 0; i < PC1MAXSECTPERARM; i++)
    {
      pc1Active[arm][i] = pc1in[i];
    }
}

void
padDetectorGeo::get_pc1Active(short arm, short pc1out[])
{
  int i;
  for (i = 0; i < PC1MAXSECTPERARM; i++)
    {
      pc1out[i] = pc1Active[arm][i];
    }
}

void
padDetectorGeo::set_pc2Active(short arm, short pc2in[])
{
  int i;
  for (i = 0; i < CGLMAXSECTPERARM; i++)
    {
      pc2Active[arm][i] = pc2in[i];
    }
}

void
padDetectorGeo::get_pc2Active(short arm, short pc2out[])
{
  int i;
  for (i = 0; i < CGLMAXSECTPERARM; i++)
    {
      pc2out[i] = pc2Active[arm][i];
    }
}

void
padDetectorGeo::set_pc3Active(short arm, short pc3in[])
{
  int i;
  for (i = 0; i < CGLMAXSECTPERARM; i++)
    {
      pc3Active[arm][i] = pc3in[i];
    }
}

void
padDetectorGeo::get_pc3Active(short arm, short pc3out[])
{
  int i;
  for (i = 0; i < CGLMAXSECTPERARM; i++)
    {
      pc3out[i] = pc3Active[arm][i];
    }
}

void
padDetectorGeo::set_xOffset(double xOffsetIn[])
{
  for (int i = 0; i < 3; i++)
    {
      xOffset[i] = xOffsetIn[i];
    }
}

void
padDetectorGeo::get_xOffset(double xOffsetOut[])
{
  for (int i = 0; i < 3; i++)
    {
      xOffsetOut[i] = xOffset[i];
    }
}

void
padDetectorGeo::set_zOffset(double zOffsetIn[])
{
  for (int i = 0; i < 3; i++)
    {
      zOffset[i] = zOffsetIn[i];
    }
}

void
padDetectorGeo::get_zOffset(double zOffsetOut[])
{
  for (int i = 0; i < 3; i++)
    {
      zOffsetOut[i] = zOffset[i];
    }
}

void
padDetectorGeo::set_gasAtten(double gasAttenIn[])
{
  for (int i = 0; i < 3; i++)
    {
      gasAtten[i] = gasAttenIn[i];
    }
}

void
padDetectorGeo::get_gasAtten(double gasAttenOut[])
{
  for (int i = 0; i < 3; i++)
    {
      gasAttenOut[i] = gasAtten[i];
    }
}

void
padDetectorGeo::set_anodeSpacing(double anodeSpacingIn[])
{
  for (int i = 0; i < 3; i++)
    {
      anodeSpacing[i] = anodeSpacingIn[i];
    }
}

void
padDetectorGeo::get_anodeSpacing(double anodeSpacingOut[])
{
  for (int i = 0; i < 3; i++)
    {
      anodeSpacingOut[i] = anodeSpacing[i];
    }
}

void
padDetectorGeo::set_pixelLength(double pixelLengthIn[])
{
  for (int i = 0; i < 3; i++)
    {
      pixelLength[i] = pixelLengthIn[i];
    }
}

void
padDetectorGeo::get_pixelLength(double pixelLengthOut[])
{
  for (int i = 0; i < 3; i++)
    {
      pixelLengthOut[i] = pixelLength[i];
    }
}

void
padDetectorGeo::set_sideWidth(double sideWidthIn[])
{
  for (int i = 0; i < 3; i++)
    {
      sideWidth[i] = sideWidthIn[i];
    }
}

void
padDetectorGeo::get_sideWidth(double sideWidthOut[])
{
  for (int i = 0; i < 3; i++)
    {
      sideWidthOut[i] = sideWidth[i];
    }
}

void
padDetectorGeo::set_centerWidth(double centerWidthIn[])
{
  for (int i = 0; i < 3; i++)
    {
      centerWidth[i] = centerWidthIn[i];
    }
}

void
padDetectorGeo::get_centerWidth(double centerWidthOut[])
{
  for (int i = 0; i < 3; i++)
    {
      centerWidthOut[i] = centerWidth[i];
    }
}

void
padDetectorGeo::set_pixelSpacing(double pixelSpacingIn[])
{
  for (int i = 0; i < 3; i++)
    {
      pixelSpacing[i] = pixelSpacingIn[i];
    }
}

void
padDetectorGeo::get_pixelSpacing(double pixelSpacingOut[])
{
  for (int i = 0; i < 3; i++)
    {
      pixelSpacingOut[i] = pixelSpacing[i];
    }
}

void
padDetectorGeo::set_cellSpacing(double cellSpacingIn[])
{
  for (int i = 0; i < 3; i++)
    {
      cellSpacing[i] = cellSpacingIn[i];
    }
}

void
padDetectorGeo::get_cellSpacing(double cellSpacingOut[])
{
  for (int i = 0; i < 3; i++)
    {
      cellSpacingOut[i] = cellSpacing[i];
    }
}

void
padDetectorGeo::set_z0Gap(double z0GapIn[])
{
  for (int i = 0; i < 3; i++)
    {
      z0Gap[i] = z0GapIn[i];
    }
}

void
padDetectorGeo::get_z0Gap(double z0GapOut[])
{
  for (int i = 0; i < 3; i++)
    {
      z0GapOut[i] = z0Gap[i];
    }
}

void
padDetectorGeo::set_nWiresPerSect(short nWiresPerSectIn[])
{
  for (int i = 0; i < 3; i++)
    {
      nWiresPerSect[i] = nWiresPerSectIn[i];
    }
}

void
padDetectorGeo::get_nWiresPerSect(short nWiresPerSectOut[])
{
  for (int i = 0; i < 3; i++)
    {
      nWiresPerSectOut[i] = nWiresPerSect[i];
    }
}

void
padDetectorGeo::set_nPadsAcrossWire(short nPadsAcrossWireIn[])
{
  for (int i = 0; i < 3; i++)
    {
      nPadsAcrossWire[i] = nPadsAcrossWireIn[i];
    }
}

void
padDetectorGeo::get_nPadsAcrossWire(short nPadsAcrossWireOut[])
{
  for (int i = 0; i < 3; i++)
    {
      nPadsAcrossWireOut[i] = nPadsAcrossWire[i];
    }
}

void
padDetectorGeo::set_nPadsAlongWire(short nPadsAlongWireIn[])
{
  for (int i = 0; i < 3; i++)
    {
      nPadsAlongWire[i] = nPadsAlongWireIn[i];
    }
}

void
padDetectorGeo::get_nPadsAlongWire(short nPadsAlongWireOut[])
{
  for (int i = 0; i < 3; i++)
    {
      nPadsAlongWireOut[i] = nPadsAlongWire[i];
    }
}

void
padDetectorGeo::set_xyz0(const PHPoint& inPoint)
{
  xyz0 = inPoint;
}

void
padDetectorGeo::set_xyz0(double inPoint[])
{
  xyz0.setX(inPoint[0]);
  xyz0.setY(inPoint[1]);
  xyz0.setZ(inPoint[2]);
}

PHPoint
padDetectorGeo::get_xyz0()
{
  return xyz0;
}

void
padDetectorGeo::set_Theta0(const PHAngle thetain[])
{
  Theta0[0] = thetain[0];
  Theta0[1] = thetain[1];
}

void
padDetectorGeo::get_Theta0(PHAngle thetaout[]) const
{
  thetaout[0] = Theta0[0];
  thetaout[1] = Theta0[1];
}

void
padDetectorGeo::set_Theta0(const double thetain[])
{
  Theta0[0] = thetain[0];
  Theta0[1] = thetain[1];
}

void
padDetectorGeo::get_Theta0(double thetaout[]) const
{
  thetaout[0] = double(Theta0[0]);
  thetaout[1] = double(Theta0[1]);
}

void
padDetectorGeo::set_PhiTop(float phiin[])
{
  PhiTop[0] = phiin[0];
  PhiTop[1] = phiin[1];
}

void
padDetectorGeo::get_PhiTop(float phiout[])
{
  phiout[0] = PhiTop[0];
  phiout[1] = PhiTop[1];
}

void
padDetectorGeo::set_PhiBottom(float phiin[])
{
  PhiBottom[0] = phiin[0];
  PhiBottom[1] = phiin[1];
}

void
padDetectorGeo::get_PhiBottom(float phiout[])
{
  phiout[0] = PhiBottom[0];
  phiout[1] = PhiBottom[1];
}

// Print out the constructed geometry
void
padDetectorGeo::PrintGeo(short detid, short arm)
{
  int i;
  double theta[2];
  // Print overall information
  cout << "padDetectorGeo:\n";
  cout << "  Verbose = " << Verbose << "\n";
  cout << "  Global Origin:\n";
  xyz0.print();
  theta[0] = Theta0[0];
  theta[1] = Theta0[1];
  cout << "  Theta0: arm 0 = " << theta[0] << " , arm 1 = " <<
  theta[1] << "\n";
  // Print PC1 information
  if (detid == 0)
    {
      cout << "  padDetectorGeo for PC1:\n";
      cout << "   Radius = " << pc1Radius << " , ZWidth = " <<
      pc1ZWidth << "\n";
      cout << "   Active sectors for arm " << arm << ": ";
      for (i = 0; i < PC1MAXSECTPERARM; i++)
        {
          cout << pc1Active[arm][i] << ",";
        }
      cout << "\n";
      cout << "   Generated planes for arm " << arm << ":\n";
      for (i = 0; i < PC1MAXSECTPERARM; i++)
        {
          if (pc1Active[arm][i] == 1)
            {
              pc1Sectors[arm][i].print();
            }
        }
    }
  // Print PC2 information
  if (detid == 1)
    {
      cout << "  padDetectorGeo for PC2:\n";
      cout << "   Radius = " << pc2Radius << " , ZWidth = " <<
      pc2ZWidth << "\n";
      cout << "   Active sectors for arm " << arm << ": ";
      for (i = 0; i < CGLMAXSECTPERARM; i++)
        {
          cout << pc2Active[arm][i] << ",";
        }
      cout << "\n";
      cout << "   Generated planes for arm " << arm << ":\n";
      for (i = 0; i < CGLMAXSECTPERARM; i++)
        {
          if (pc2Active[arm][i] == 1)
            {
              if (isFromDatabase())
                {
                  for (short side = 0; side < 2; side++)
                    {
                      pc2Chambers[arm][i][side].print();
                    }
                }
              else
                {
                  pc2Sectors[arm][i].print();
                }
            }
        }
    }
  // Print PC3 information
  if (detid == 2)
    {
      cout << "  padDetectorGeo for PC3:\n";
      cout << "   Radius = " << pc3Radius << " , ZWidth = " <<
      pc3ZWidth << "\n";
      cout << "   Active sectors for arm " << arm << ": ";
      for (i = 0; i < CGLMAXSECTPERARM; i++)
        {
          cout << pc3Active[arm][i] << ",";
        }
      cout << "\n";
      cout << "   Generated planes for arm " << arm << ":\n";
      for (i = 0; i < CGLMAXSECTPERARM; i++)
        {
          if (pc3Active[arm][i] == 1)
            {
              if (isFromDatabase())
                {
                  for (short side = 0; side < 2; side++)
                    {
                      pc3Chambers[arm][i][side].print();
                    }
                }
              else
                {
                  pc3Sectors[arm][i].print();
                }
            }
        }
    }
}

// Print the parameter information from dPadGeom
void
padDetectorGeo::PrintParams()
{
  int i, j;
  cout << "padDetectorGeo Parameters: \n";
  cout << "  pc1Radius = " << pc1Radius << "\n";
  cout << "  pc2Radius = " << pc2Radius << "\n";
  cout << "  pc3Radius = " << pc3Radius << "\n";
  for (i = 0; i < 3; i++)
    {
      j = i + 1;
      cout << "  Pad Chamber " << j << ":\n";
      cout << "    xOffset = " << xOffset[i] << "\n";
      cout << "    zOffset = " << zOffset[i] << "\n";
      cout << "    gasAtten = " << gasAtten[i] << "\n";
      cout << "    anodeSpacing = " << anodeSpacing[i] << "\n";
      cout << "    pixelLength = " << pixelLength[i] << "\n";
      cout << "    sideWidth = " << sideWidth[i] << "\n";
      cout << "    centerWidth = " << centerWidth[i] << "\n";
      cout << "    pixelSpacing = " << pixelSpacing[i] << "\n";
      cout << "    cellSpacing = " << cellSpacing[i] << "\n";
      cout << "    z0Gap = " << z0Gap[i] << "\n";
      cout << "    nWiresPerSect = " << nWiresPerSect[i] << "\n";
      cout << "    nPadsAcrossWire = " << nPadsAcrossWire[i] << "\n";
      cout << "    nPadsAlongWire = " << nPadsAlongWire[i] << "\n";
    }
}

// additions by Julia Velkovska to apply geometry transformations
void
padDetectorGeo::set_phenixFrame(PHFrame& ph)
{
  phenixFrame = ph;
}

PHFrame
padDetectorGeo::get_phenixFrame()
{
  return phenixFrame;
}

void
padDetectorGeo::set_eastFrame(PHFrame& ph)
{
  eastFrame = ph;
  cout << " padDetectorGeo::set_eastFrame to " << endl;
  eastFrame.print();
}

PHFrame
padDetectorGeo::get_eastFrame()
{
  return eastFrame;
}

void
padDetectorGeo::set_westFrame(PHFrame& ph)
{
  westFrame = ph;
  cout << " padDetectorGeo::set_westFrame to " << endl;
  westFrame.print();
}

PHFrame
padDetectorGeo::get_westFrame()
{
  return westFrame;
}

//  individual sector/chamber frames
void
padDetectorGeo::set_pc1Frame(PHFrame& ph, short &pc1arm, short &pc1sector)
{
  pc1SectorFrame[pc1arm][pc1sector] = ph;
}

PHFrame
padDetectorGeo::get_pc1Frame(short &pc1arm, short &pc1sector)
{
  return pc1SectorFrame[pc1arm][pc1sector];
}

void
padDetectorGeo::set_pc2Frame(PHFrame& ph, short &pc2arm, short &pc2sector)
{
  pc2SectorFrame[pc2arm][pc2sector] = ph;
}

void
padDetectorGeo::set_pc2Frame(PHFrame& ph, short &pc2arm, short &pc2sector, short &side)
{
  pc2ChamberFrame[pc2arm][pc2sector][side] = ph;
}

PHFrame
padDetectorGeo::get_pc2Frame(short &pc2arm, short &pc2sector)
{
  return pc2SectorFrame[pc2arm][pc2sector];
}

PHFrame
padDetectorGeo::get_pc2Frame(short &pc2arm, short &pc2sector, short &side)
{
  return pc2ChamberFrame[pc2arm][pc2sector][side];
}

void
padDetectorGeo::set_pc3Frame(PHFrame& ph, short &pc3arm, short &pc3sector)
{
  pc3SectorFrame[pc3arm][pc3sector] = ph;
}

void
padDetectorGeo::set_pc3Frame(PHFrame& ph, short &pc3arm, short &pc3sector, short &side)
{
  pc3ChamberFrame[pc3arm][pc3sector][side] = ph;
}

PHFrame
padDetectorGeo::get_pc3Frame(short &pc3arm, short &pc3sector)
{
  return pc3SectorFrame[pc3arm][pc3sector];
}

PHFrame
padDetectorGeo::get_pc3Frame(short &pc3arm, short &pc3sector, short &side)
{
  return pc3ChamberFrame[pc3arm][pc3sector][side];
}

void
padDetectorGeo::rotateAndTranslate()
{
  PHFrame armFrame = phenixFrame;
  PHPanel temp;
  for (int arm = 0;arm < 2;arm++)
    {
      if (arm == 0)
        {
          armFrame = get_eastFrame();
        }
      if (arm == 1)
        {
          armFrame = westFrame;
        }
      for (int sector = 0;sector < PC1MAXSECTPERARM;sector++)
        {
          // first transform into the panel's frame
          temp = transformPanel(phenixFrame, pc1Sectors[arm][sector], pc1SectorFrame[arm][sector]);
          // next transform into the arm
          pc1Sectors[arm][sector] = transformPanel(phenixFrame, temp, armFrame);
        }
      for (int sector = 0;sector < CGLMAXSECTPERARM;sector++)
        {
          if (isFromDatabase())
            {
              for (int side = 0;side < 2;side++)
                {
                  temp = transformPanel(phenixFrame, pc2Chambers[arm][sector][side], pc2ChamberFrame[arm][sector][side]);
                  // next transform into the arm
                  pc2Chambers[arm][sector][side] = transformPanel(phenixFrame, temp, armFrame);
                  temp = transformPanel(phenixFrame, pc3Chambers[arm][sector][side], pc3ChamberFrame[arm][sector][side]);
                  // next transform into the arm
                  pc3Chambers[arm][sector][side] = transformPanel(phenixFrame, temp, armFrame);
                }
            }
          else
            {
              // first transform into the panel's frame
              temp = transformPanel(phenixFrame, pc2Sectors[arm][sector], pc2SectorFrame[arm][sector]);
              // next transform into the arm
              pc2Sectors[arm][sector] = transformPanel(phenixFrame, temp, armFrame);
              // first transform into the panel's frame
              temp = transformPanel(phenixFrame, pc3Sectors[arm][sector], pc3SectorFrame[arm][sector]);
              // next transform into the arm
              PHPanel tempArm = transformPanel(phenixFrame, temp, armFrame);
              // pc3Sectors[arm][sector]= transformPanel(phenixFrame,temp,armFrame);
              pc3Sectors[arm][sector] = tempArm;
            }
        }
    }
}
