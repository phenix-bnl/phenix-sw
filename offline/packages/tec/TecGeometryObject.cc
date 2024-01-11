// Created by: Sasha Lebedev (ISU) lebedev@iastate.edu 05/20/99
// Description: Implementation of TecGeometryObject class

#include <iostream>
#include <fstream>
#include <cstdlib>
#include "gsl/gsl_math.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "TecGeometryObject.hh" 

using namespace std;
using namespace PHGeometry;

/// Constructor
TecGeometryObject::TecGeometryObject()
{

  Debug=0;
  iFlag=-1;
  CalibName = "geom.tec.run00";
  setBankNumber(TECGEOMBANK);
  setBankID(BankNumber);
  setDescription("Tec Geometry");

  Tsearch.setToSystemTime();

  PHFrame XYZ; // this builds a default coordinate system
  EastCarriage = XYZ;
  PHPanel ZERO;
  for(int i=0; i<TECMAXINDEX; i++) { TecPlaneFrame[i] = XYZ; TecPlane_[i] = ZERO; tecplane_calculated[i] = false; ActivePlane[i]=0; }
  for(int i=0; i<TECMAXSECT; i++) { TecSectorFrame[i] = XYZ; TecPlane_[i] = ZERO; tecplane_calculated[i] = false;}

  ActiveSectorSide[0]=True;
  ActiveSectorSide[1]=True;
  ActiveSectorSide[2]=True;
  ActiveSectorSide[3]=True;
  ActiveSectorSide[4]=True;
  ActiveSectorSide[5]=True;
  ActiveSectorSide[6]=True;
  ActiveSectorSide[7]=True;

  for(int i=0; i<TECMAXSECT; i++) {
    for(int k=0; k<TECMAXSIDE; k++) {
      for(int j=0; j<TECMAXPLANE; j++) {
        int myindex=i*TECMAXPLANE*TECMAXSIDE+j*TECMAXSIDE+k;
//        if((i==1 || i==2) && j<4) {
//        if(j<4) {
          ActivePlane[myindex]=1;
//        } 
      }
    }
  }

  for(int i=0; i<TECMAXINDEX; i++) {
    for(int j=0; j<TECMAXWIRE; j++) {
      GlobalX[i][j]=0.;
      GlobalY[i][j]=0.;
    }
  }
 
}

/// Destructor
TecGeometryObject::~TecGeometryObject() {}

void 
TecGeometryObject::UseSimulationDatabase() 
{
  CalibName = "geom.tec.geant00";
}

void 
TecGeometryObject::UseRealDatabase() 
{
  CalibName = "geom.tec.run00";
}

/// Set East Carriage Position
PHBoolean 
TecGeometryObject::setEastCarriage(PHFrame &f)
{
  EastCarriage = f;
  return True;
}

/// Set TecPlane Frame
PHBoolean 
TecGeometryObject::setTecPlaneFrame(int index, PHFrame &f)
{
  TecPlaneFrame[index]=f;
  return True;
}

/// Set TecSector Frame
PHBoolean 
TecGeometryObject::setTecSectorFrame(int index, PHFrame &f)
{
  TecSectorFrame[index]=f;
  return True;
}

/// Get TecPlane taking into account rotations and shifts
PHPanel 
TecGeometryObject::getTecPanel(int index)
{
// The following frame will rotate sector or plane clockwise around its
// center by 10 milliradians and shift it by 5 cm in the direction of X axis.
//    float shiftx = 5.0;
//    float rotangle = 0.01;
//    PHPoint origin = PHPoint(shiftx,0.0,0.0);
//    float rotcos = cos(rotangle);
//    float rotsin = sin(rotangle);
//    PHVector xaxisrot = PHVector(rotcos,-rotsin,0.);
//    PHVector yaxisrot = PHVector(rotsin,rotcos,0.);
//    PHVector zaxisrot = PHVector(0.,0.,1.);
//    PHFrame shiftrot = PHFrame(origin,xaxisrot,yaxisrot,zaxisrot);
//

  if (tecplane_calculated[index])
    return TecPlane_[index];

  PHFrame XYZ;
  int sector = index/TECMAXPLANE/TECMAXSIDE;
  float xcenter[TECMAXPLANE*TECMAXSIDE];
  float ycenter[TECMAXPLANE*TECMAXSIDE];
  float zcenter[TECMAXPLANE*TECMAXSIDE];
  float Xcenter,Ycenter,Zcenter;

// find sector center
  Xcenter=0.; Ycenter=0.; Zcenter=0.;
    for(int i=0; i<TECMAXPLANE*TECMAXSIDE; i++) {
      xcenter[i] = ((TecPlane[i+sector*TECMAXPLANE*TECMAXSIDE]).getCenter()).getX();
      ycenter[i] = ((TecPlane[i+sector*TECMAXPLANE*TECMAXSIDE]).getCenter()).getY();
      zcenter[i] = ((TecPlane[i+sector*TECMAXPLANE*TECMAXSIDE]).getCenter()).getZ();
      Xcenter += xcenter[i];
      Ycenter += ycenter[i];
      Zcenter += zcenter[i];
    }
    Xcenter = Xcenter/(TECMAXPLANE*TECMAXSIDE);
    Ycenter = Ycenter/(TECMAXPLANE*TECMAXSIDE);
    Zcenter = Zcenter/(TECMAXPLANE*TECMAXSIDE);
    PHPoint SectorCenter = PHPoint(Xcenter,Ycenter,Zcenter);

// transform sector
  PHFrame XYZ1 = TecSectorFrame[sector];
  PHFrame XYZ2;
  PHFrame XYZ3;
  PHPoint SectorShift = (TecSectorFrame[sector]).getOrigin();
  XYZ1.setOrigin(SectorCenter);
  XYZ2.setOrigin(SectorCenter);
  XYZ3.setOrigin(SectorShift);

// rotate around sector center
  PHPanel TmpPanel1 = transformPanel(XYZ,(TecPlane[index]),XYZ1);
  PHPanel TmpPanel2 = transformPanel(XYZ2,TmpPanel1,XYZ);
// shift
  PHPanel NewPanel1 = transformPanel(XYZ3,TmpPanel2,XYZ);


// transform plane 
  PHPoint PlaneCenter = (TecPlane[index]).getCenter();
  PHFrame XYZ4 = TecPlaneFrame[index];
  PHFrame XYZ5;
  PHFrame XYZ6;
  PHPoint PlaneShift = (TecPlaneFrame[index]).getOrigin();
  XYZ4.setOrigin(PlaneCenter);
  XYZ5.setOrigin(PlaneCenter);
  XYZ6.setOrigin(PlaneShift);

// rotate around plane center
  PHPanel TmpPanel3 = transformPanel(XYZ,(NewPanel1),XYZ4);
  PHPanel TmpPanel4 = transformPanel(XYZ5,TmpPanel3,XYZ);
// shift
  PHPanel NewPanel2 = transformPanel(XYZ6,TmpPanel4,XYZ);

// transform carriage (rotation will be around (0,0,0))
  PHPanel panelOut = transformPanel(XYZ, NewPanel2, EastCarriage);

  TecPlane_[index] = panelOut;
  tecplane_calculated[index] = true;
  return panelOut;

}

/// Calculate wire X coordinate in global Phenix coordinate system
float 
TecGeometryObject::calculateGlobalX(int index, int iwire) 
{
  static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecGeometryObject ERROR: TGO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
  }

    PHPanel PNL = getTecPanel(index);
    PHPoint PNT0 = PNL.getPoint(0);
    PHPoint PNT1 = PNL.getPoint(1);
    PHPoint PNT2 = PNL.getPoint(2);
    PHVector VCT = PNL.getNormal();
      double cosphi = -VCT.getX();
      double sinphi = -VCT.getY();
      double X0 = PNT1.getX() + (PNT0.getX()-PNT1.getX())/2.;

        PHPoint SC = getSouthCoordinate(index, iwire);
        PHPoint NC = getNorthCoordinate(index, iwire);
        double X1 = X0 + cosphi*SC.getX() - sinphi*SC.getY();
        double X2 = X0 + cosphi*NC.getX() - sinphi*NC.getY();

  return (X1+X2)/2.0;
} 

/// Calculate wire Y coordinate in global Phenix coordinate system
float 
TecGeometryObject::calculateGlobalY(int index, int iwire) 
{
  static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecGeometryObject ERROR: TGO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
  }

    PHPanel PNL = getTecPanel(index);
    PHPoint PNT0 = PNL.getPoint(0);
    PHPoint PNT1 = PNL.getPoint(1);
    PHPoint PNT2 = PNL.getPoint(2);
    PHVector VCT = PNL.getNormal();
      double cosphi = -VCT.getX();
      double sinphi = -VCT.getY();
      double Y0 = PNT1.getY() + (PNT0.getY()-PNT1.getY())/2.;

        PHPoint SC = getSouthCoordinate(index, iwire);
        PHPoint NC = getNorthCoordinate(index, iwire);
        double Y1 = Y0 + cosphi*SC.getY() + sinphi*SC.getX();
        double Y2 = Y0 + cosphi*NC.getY() + sinphi*NC.getX();

  return (Y1+Y2)/2.;
}


/// Get south wire coordinate
PHPoint 
TecGeometryObject::getSouthCoordinate(TecAddressObject* TAO) 
{
  static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecGeometryObject ERROR: TGO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
  }
  int isector = TAO->getSector();
  int iplane = TAO->getPlane();
  int iside = TAO->getSide();
  int iwire = TAO->getWire();
  int index = iside + iplane*TECMAXSIDE + isector*TECMAXSIDE*TECMAXPLANE;
 
  return SouthCoordinate[index][iwire];
}

/// Get south wire coordinate
PHPoint TecGeometryObject::getSouthCoordinate(int index, int iwire) {
  static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecGeometryObject ERROR: TGO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
  }

  return SouthCoordinate[index][iwire];
}


/// Get north wire coordinate
PHPoint TecGeometryObject::getNorthCoordinate(TecAddressObject* TAO) {
  static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecGeometryObject ERROR: TGO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
  }
  int isector = TAO->getSector();
  int iplane = TAO->getPlane();
  int iside = TAO->getSide();
  int iwire = TAO->getWire();
  int index = iside + iplane*TECMAXSIDE + isector*TECMAXSIDE*TECMAXPLANE;

  return NorthCoordinate[index][iwire];
}

/// Get north wire coordinate
PHPoint 
TecGeometryObject::getNorthCoordinate(int index, int iwire) 
{
  static int kuku=0;
  if(iFlag!=0 && kuku==0) {
    cerr << "TecGeometryObject ERROR: TGO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    kuku=1;
  }

  return NorthCoordinate[index][iwire];
}

/// Fetch Sector Frames from an ASCII files
PHBoolean 
TecGeometryObject::FetchSectorFramesFromFile(const char* fname) 
{
  
  int userotangle = 0;

  double rotangle,buff[12];
  PHVector xaxisrot = PHVector(1.,0.,0.);
  PHVector yaxisrot = PHVector(0.,1.,0.);
  PHVector zaxisrot = PHVector(0.,0.,1.);
  PHPoint center = PHPoint(0.,0.,0.);

  ifstream file;
    file.open(fname);
    
    if(!file) {
      cerr << "TecGeometryObject::FetchSectorFramesFromFile WARNING: "
           << "Can not open ascii file " << fname << endl;
      return False;
    }
    else {
      // Read Sector Frames
      for(int i=0; i<TECMAXSECT; i++) {
        file >> rotangle;
        for(int j=0; j<12; j++) { file >> buff[j]; }
       
          if(userotangle==1) {
            center.setX(0.);
            center.setY(0.);
            center.setZ(0.);
              xaxisrot.setX(cos(rotangle));
              xaxisrot.setY(-sin(rotangle));
              xaxisrot.setZ(0.);
                yaxisrot.setX(sin(rotangle));
                yaxisrot.setY(cos(rotangle));
                yaxisrot.setZ(0.);
                  zaxisrot.setX(0.);
                  zaxisrot.setY(0.);
                  zaxisrot.setZ(1.);
          }
          else {
	    center.setX(buff[0]);
            center.setY(buff[1]);
            center.setZ(buff[2]);
              xaxisrot.setX(buff[3]);
              xaxisrot.setY(buff[4]);
              xaxisrot.setZ(buff[5]);
                yaxisrot.setX(buff[6]);
                yaxisrot.setY(buff[7]);
                yaxisrot.setZ(buff[8]);
                  zaxisrot.setX(buff[9]);
                  zaxisrot.setY(buff[10]);
                  zaxisrot.setZ(buff[11]);
          }

	    TecSectorFrame[i] = PHFrame(center,xaxisrot,yaxisrot,zaxisrot);

        if(Debug>0) {
          cout << "Sector Frame # " << i << endl;
          cout << ((TecSectorFrame[i]).getOrigin()).getX() << " "
               << ((TecSectorFrame[i]).getOrigin()).getY() << " "
               << ((TecSectorFrame[i]).getOrigin()).getZ() << endl;
          cout << ((TecSectorFrame[i]).getU()).getX() << " "
               << ((TecSectorFrame[i]).getU()).getY() << " "
               << ((TecSectorFrame[i]).getU()).getZ() << endl;
          cout << ((TecSectorFrame[i]).getV()).getX() << " "
               << ((TecSectorFrame[i]).getV()).getY() << " "
               << ((TecSectorFrame[i]).getV()).getZ() << endl;
          cout << ((TecSectorFrame[i]).getW()).getX() << " "
               << ((TecSectorFrame[i]).getW()).getY() << " "
               << ((TecSectorFrame[i]).getW()).getZ() << endl;
        }

      } // read sector frames

// Read Plane Frames
      for(int i=0; i<TECMAXINDEX; i++) {
        file >> rotangle;
        for(int j=0; j<12; j++) { file >> buff[j]; }

            center.setX(buff[0]);
            center.setY(buff[1]);
            center.setZ(buff[2]);
              xaxisrot.setX(buff[3]);
              xaxisrot.setY(buff[4]);
              xaxisrot.setZ(buff[5]);
                yaxisrot.setX(buff[6]);
                yaxisrot.setY(buff[7]);
                yaxisrot.setZ(buff[8]);
                  zaxisrot.setX(buff[9]);
                  zaxisrot.setY(buff[10]);
                  zaxisrot.setZ(buff[11]);

            TecPlaneFrame[i] = PHFrame(center,xaxisrot,yaxisrot,zaxisrot);

        if(Debug>0) {
          cout << "Plane Frame # " << i << endl;
          cout << ((TecPlaneFrame[i]).getOrigin()).getX() << " "
               << ((TecPlaneFrame[i]).getOrigin()).getY() << " "
               << ((TecPlaneFrame[i]).getOrigin()).getZ() << endl;
          cout << ((TecPlaneFrame[i]).getU()).getX() << " "
               << ((TecPlaneFrame[i]).getU()).getY() << " "
               << ((TecPlaneFrame[i]).getU()).getZ() << endl;
          cout << ((TecPlaneFrame[i]).getV()).getX() << " "
               << ((TecPlaneFrame[i]).getV()).getY() << " "
               << ((TecPlaneFrame[i]).getV()).getZ() << endl;
          cout << ((TecPlaneFrame[i]).getW()).getX() << " "
               << ((TecPlaneFrame[i]).getW()).getY() << " "
               << ((TecPlaneFrame[i]).getW()).getZ() << endl;
        }

      } // read plane frames

// Read Arm Frame
        file >> rotangle;
        for(int j=0; j<12; j++) { file >> buff[j]; }

            center.setX(buff[0]);
            center.setY(buff[1]);
            center.setZ(buff[2]);
              xaxisrot.setX(buff[3]);
              xaxisrot.setY(buff[4]);
              xaxisrot.setZ(buff[5]);
                yaxisrot.setX(buff[6]);
                yaxisrot.setY(buff[7]);
                yaxisrot.setZ(buff[8]);
                  zaxisrot.setX(buff[9]);
                  zaxisrot.setY(buff[10]);
                  zaxisrot.setZ(buff[11]);

            EastCarriage = PHFrame(center,xaxisrot,yaxisrot,zaxisrot);


        if(Debug>0) {
          cout << "East Arm Frame: " << endl;
          cout << ((EastCarriage).getOrigin()).getX() << " "
               << ((EastCarriage).getOrigin()).getY() << " "
               << ((EastCarriage).getOrigin()).getZ() << endl;
          cout << ((EastCarriage).getU()).getX() << " "
               << ((EastCarriage).getU()).getY() << " "
               << ((EastCarriage).getU()).getZ() << endl;
          cout << ((EastCarriage).getV()).getX() << " "
               << ((EastCarriage).getV()).getY() << " "
               << ((EastCarriage).getV()).getZ() << endl;
          cout << ((EastCarriage).getW()).getX() << " "
               << ((EastCarriage).getW()).getY() << " "
               << ((EastCarriage).getW()).getZ() << endl;

      } // read arm frame


    } // file exists
  
  return True;
}

/// Fetch a list of ActivePlanes from an ASCII files
PHBoolean 
TecGeometryObject::FetchActivePlanesFromFile(const char* fname) 
{

  int activeplane,activess;
  ifstream file;
    file.open(fname);

    if(!file) {
      cerr << "TecGeometryObject::FetchActivePlanesFromFile ERROR: "
           << "Can not open ascii file " << fname << endl;
      return False;
    }
    else {
      for(int i=0; i<TECMAXINDEX; i++) {
        file >> activeplane;
        ActivePlane[i]=activeplane;
      }
      for(int i=0; i<TECMAXSECT*TECMAXSIDE; i++) {
        file >> activess;
        ActiveSectorSide[i]=activess;
      }
    }

  return True;
}

/// Fetch Tec Planes geometry from an ASCII files
PHBoolean 
TecGeometryObject::FetchPanelsFromFile(const char* fname) 
{

  const char *tmpfname = fname;
  ifstream file;
  float xx[4],yy[4],zz[4];
  PHPoint points[4];

// Correct (in memory) order of points in panel is:
// 1. Top South
// 2. Bottom South
// 3. Top North
// Actuall order of points in database and ascii files is:
// 1. Bottom South
// 2. Top South
// 3. Top North

  if(Debug>0) cout << "TecGeometryObject::FetchPanelsFromFile: Reading "
                    << tmpfname << endl;

    file.open(tmpfname);

    if(!file) {
      cerr << "TecGeometryObject::FetchPanelsFromFile ERROR: Can not open ascii file "
           << tmpfname << endl;
      return False;
    }
    else {
        for(int i=0; i<TECMAXINDEX; i++) {
          for(int ip=0; ip<4; ip++) {
            file >> xx[ip] >> yy[ip] >> zz[ip];
            points[ip] = PHPoint(xx[ip],yy[ip],zz[ip]);
            if(ip==2) {
              TecPlane[i] = PHPanel(points[1],points[0],points[2]);
              if(Debug>1) {
              cout << i << " " << ((TecPlane[i]).getPoint(0)).getX() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(0)).getY() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(0)).getZ() << " " << endl;
              cout << i << " " << ((TecPlane[i]).getPoint(1)).getX() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(1)).getY() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(1)).getZ() << " " << endl;
              cout << i << " " << ((TecPlane[i]).getPoint(2)).getX() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(2)).getY() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(2)).getZ() << " " << endl;
              cout << i << " " << ((TecPlane[i]).getPoint(3)).getX() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(3)).getY() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(3)).getZ() << " " << endl;
              }
            }
          }
        }
      file.close();
    }

// If relative wire coordinates were already read, calculate global 
// coordinates of all wires again
   if(iFlag==0) {
     for(int index=0; index<TECMAXINDEX; index++) {
       int sector = index/(TECMAXPLANE*TECMAXSIDE);
       int plane = (index - sector*TECMAXPLANE*TECMAXSIDE)/TECMAXSIDE;
       for(int i=0; i<TecGeometryObject::get_NumWires(plane); i++) {
           GlobalX[index][i] = calculateGlobalX(index,i);
           GlobalY[index][i] = calculateGlobalY(index,i);
       }
     }
   }

  return True;
}

/// Fetch geometry information from a set of ASCII files
PHBoolean 
TecGeometryObject::FetchFromFile(const char* fname) 
{

// Read files with geometry

  char *tmpfname = new char[strlen(fname)+1];
  strcpy(tmpfname,fname);
  ifstream file;
  float xx[4],yy[4],zz[4];
  float x,y1,y2,z1,z2;
  PHPoint points[4];
  int kk1,kk2,iplane;
  char cc1,cc2;

  for(int ifile=TECMAXINDEX; ifile>=0; ifile--) {

  if(ifile<10) { kk1 = 0; kk2 = ifile; }
    else { kk1 = ifile/10; kk2 = ifile - kk1*10; }
  cc1 = (char)(kk1+48); cc2 = (char)(kk2+48);
  tmpfname[23]=cc1; tmpfname[24]=cc2;

   if(Debug>0) cout << "TecGeometryObject::FetchFromFile: Reading " 
                    << tmpfname << endl;

// Correct (in memory) order of points in panel is:
// 1. Top South
// 2. Bottom South
// 3. Top North
// Actuall order of points in database and ascii files is:
// 1. Bottom South
// 2. Top South
// 3. Top North

    file.open(tmpfname);

    if(!file) {
      cerr << "TecGeometryObject::FetchFromFile ERROR: Can not open ascii file " 
           << tmpfname << endl;
      delete [] tmpfname;
      return False;
    }
    else {

      if(ifile==TECMAXINDEX) {
        for(int i=0; i<TECMAXINDEX; i++) {
          for(int ip=0; ip<4; ip++) {
            file >> xx[ip] >> yy[ip] >> zz[ip];
            points[ip] = PHPoint(xx[ip],yy[ip],zz[ip]);
            if(ip==2) { 
              TecPlane[i] = PHPanel(points[1],points[0],points[2]);
              if(Debug>1) {
              cout << i << " " << ((TecPlane[i]).getPoint(0)).getX() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(0)).getY() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(0)).getZ() << " " << endl;
              cout << i << " " << ((TecPlane[i]).getPoint(1)).getX() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(1)).getY() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(1)).getZ() << " " << endl;
              cout << i << " " << ((TecPlane[i]).getPoint(2)).getX() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(2)).getY() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(2)).getZ() << " " << endl;
              cout << i << " " << ((TecPlane[i]).getPoint(3)).getX() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(3)).getY() << " ";
              cout << i << " " << ((TecPlane[i]).getPoint(3)).getZ() << " " << endl;
	      }
            }
          }
        }
      }
      else {
        iplane = (ifile%12)/2;
        x=0.;
        z1 = ((TecPlane[ifile]).getPoint(0)).getZ();
        z2 = ((TecPlane[ifile]).getPoint(2)).getZ();
	for(int i=0; i<TecGeometryObject::get_NumWires(iplane); i++) {
          file >> y1 >> y2;
	  SouthCoordinate[ifile][i] = PHPoint(x,y1,z1);
	  NorthCoordinate[ifile][i] = PHPoint(x,y2,z2);
        }
      }

      file.close();

    } // if file opened

  } // end ifile loop

  iFlag = 0;

// Fetch Sector Frames

  if(Debug>0) cout << "Reading Sector Frames." << endl;
  cc1 = (char)(5+48); cc2 = (char)(0+48);
  tmpfname[23]=cc1; tmpfname[24]=cc2;
  PHBoolean statussf = FetchSectorFramesFromFile(tmpfname);
  if(!statussf) {
    cerr << "TecGeometryObject::FetchFromFile: WARNING:"
         << "Can not fetch Sector Frames. Default will be used." << endl;
  }

// Calculate global coordinates of wires
     for(int index=0; index<TECMAXINDEX; index++) {
       int sector = index/(TECMAXPLANE*TECMAXSIDE);
       int plane = (index - sector*TECMAXPLANE*TECMAXSIDE)/TECMAXSIDE;
       for(int i=0; i<TecGeometryObject::get_NumWires(plane); i++) {
           GlobalX[index][i] = calculateGlobalX(index,i);
           GlobalY[index][i] = calculateGlobalY(index,i);
       }
     }

// Fetch a list of ActivePlanes

  if(Debug>0) cout << "Reading Active Planes List." << endl;
  cc1 = (char)(4+48); cc2 = (char)(9+48);
  tmpfname[23]=cc1; tmpfname[24]=cc2;
  PHBoolean statusap = FetchActivePlanesFromFile(tmpfname);
  if(!statusap) {
    cerr << "TecGeometryObject::FetchFromFile: WARNING:"
         << "Can not fetch the list of ActivePlanes. Default will be used." << endl; 
  }

  delete [] tmpfname;
  return True;
}

/// Fetch geometry information from an ASCII file "phnx.par"
PHBoolean 
TecGeometryObject::FetchFromPHNXPAR(const char* fname) 
{

  int i,index,isect,iplane,izsign,iwire;
  int firstplane,lastplane;
  float rwire,halfang,dphi,phi1,phi2;
  float MidWire,rasst0,Xwire,Ywire,Zwire1,Zwire2;

  float R0=424.2,ThRad=7.065,ThXe=3.7032,AngS=11.2,ThickF=1.94;
  float ThickX=6.35,ThickZ=0.6,Ysup=3.5,ThickS=0.05,ThickS1=0.076,ThSLU=0.3; 
  float ThStr=0.1,ThWin=0.0050,ThWS=0.076,XWS=0.076,SST=25.4,Zsz0[6];
  int Nsect=4,WestArm=0,EastArm=1,Ltec[6];

// Read phnx.par file

  char line[120]; char tmp[9] = "        "; char* dummy[1];
  const char *phnxpar = fname;
  ifstream file;
  char tmpstr[9] = "$trd_par";
  if(Debug>0) cout << endl << "Reading " << phnxpar << endl << endl;
  file.open(phnxpar);
  if(!file)
  {
    cerr << "TecGeometryObject::FetchFromPHNXPAR ERROR: Can not open ascii file" << phnxpar << endl;
    return False;
  }
  else
  {
    for(i=0; i<9999; i++) {
      file >> line;
      if (file.eof()) break;
      if(!strncmp(tmpstr,line,8)) {
        if(Debug>1) cout << "  List of TRD_PAR:" << endl;

        file >> line; file >> line; file >> line;
        WestArm = strtol(line,dummy,16);
        if(Debug>1) cout << "WestArm = " << WestArm << endl;

        file >> line; file >> line; file >> line;
        EastArm = strtol(line,dummy,16);
        if(Debug>1) cout << "EastArm = " << EastArm << endl;

        file >> line; file >> line; file >> line;
        Nsect = strtol(line,dummy,16);
        if(Debug>1) cout << "Nsect = " << Nsect << endl;

        file >> line; file >> line; file >> line;
        R0 = strtod(line,dummy);
        if(Debug>1) cout << "R0 = " << R0 << endl;

        file >> line; file >> line; file >> line;
        ThRad = strtod(line,dummy);
        if(Debug>1) cout << "ThRad = " << ThRad << endl;

        file >> line; file >> line; file >> line;
        ThXe = strtod(line,dummy);
        if(Debug>1) cout << "ThXe = " << ThXe << endl;

        file >> line; file >> line; file >> line;
        AngS = strtod(line,dummy);
        if(Debug>1) cout << "AngS = " << AngS << endl;

        file >> line; file >> line;
        if(Debug>1) cout << "Ltec = ";
        for(i=0; i<6; i++) {
          file >> line; strncpy(tmp,line,1); 
          Ltec[i] = strtol(tmp,dummy,16);
          if(Debug>1) cout << Ltec[i] << ", ";
        }
        if(Debug>1) cout << endl;

        file >> line; file >> line; file >> line;
        ThickF = strtod(line,dummy);
        if(Debug>1) cout << "ThickF = " << ThickF << endl;

        file >> line; file >> line; file >> line;
        ThickX = strtod(line,dummy);
        if(Debug>1) cout << "ThickX = " << ThickX << endl;

        file >> line; file >> line; file >> line;
        ThickZ = strtod(line,dummy);
        if(Debug>1) cout << "ThickZ = " << ThickZ << endl;

        file >> line; file >> line; file >> line;
        Ysup = strtod(line,dummy);
        if(Debug>1) cout << "Ysup = " << Ysup << endl;

        file >> line; file >> line; file >> line;
        ThickS = strtod(line,dummy);
        if(Debug>1) cout << "ThickS = " << ThickS << endl;

        file >> line; file >> line; file >> line;
        ThickS1 = strtod(line,dummy);
        if(Debug>1) cout << "ThickS1 = " << ThickS1 << endl;

        file >> line; file >> line; file >> line;
        ThSLU = strtod(line,dummy);
        if(Debug>1) cout << "ThSLU = " << ThSLU << endl;

        file >> line; file >> line; file >> line;
        ThStr = strtod(line,dummy);
        if(Debug>1) cout << "ThStr = " << ThStr << endl;

        file >> line; file >> line; file >> line;
        ThWin = strtod(line,dummy);
        if(Debug>1) cout << "ThWin = " << ThWin << endl;

        file >> line; file >> line; file >> line;
        ThWS = strtod(line,dummy);
        if(Debug>1) cout << "ThWS = " << ThWS << endl;

        file >> line; file >> line; file >> line;
        XWS = strtod(line,dummy);
        if(Debug>1) cout << "XWS = " << XWS << endl;

        file >> line; file >> line; file >> line;
        SST = strtod(line,dummy);
        if(Debug>1) cout << "SST = " << SST << endl;

        file >> line; file >> line;
        if(Debug>1) cout << "Zsz0 = ";
        for(i=0; i<6; i++) {
          file >> line; strncpy(tmp,line,8); 
          Zsz0[i] = strtod(tmp,dummy);
          if(Debug>1) cout << Zsz0[i] << ", ";
        }
        if(Debug>1) cout << endl;
        break;
      }
    }
  }

  float dTecGeom_phitopw,dTecGeom_phibotw,dTecGeom_phitope,dTecGeom_phibote;
  float dTecGeom_Radius[6],dTecGeom_WireRadius[6],dTecGeom_Zwidth[6];
  int dTecGeom_sectperarm;

  if(Debug>1) cout << endl;
  dTecGeom_sectperarm = Nsect;
  if(Debug>1) cout << dTecGeom_sectperarm << " " << TecGeometryObject::get_sectperarm() << endl;
  dTecGeom_phitopw = 5.0*AngS;
  if(Debug>1) cout << dTecGeom_phitopw << " " << TecGeometryObject::get_phitopw() << endl;
  dTecGeom_phibotw = -3.0*AngS;
  if(Debug>1) cout << dTecGeom_phibotw << " " << TecGeometryObject::get_phibotw() << endl;
  dTecGeom_phitope = 180.0 - 5.0*AngS;
  if(Debug>1) cout << dTecGeom_phitope << " " << TecGeometryObject::get_phitope() << endl;
  dTecGeom_phibote = 180.0 + 3.0*AngS;
  if(Debug>1) cout << dTecGeom_phibote << " " << TecGeometryObject::get_phibote() << endl;
  if(Debug>1) cout << endl;
  for(i=0; i<TECMAXPLANE; i++) {
    dTecGeom_Radius[i] = R0 + ThRad + ThXe/2. + i*(ThRad+ThXe);
    dTecGeom_WireRadius[i] = R0 + (i+1)*(ThRad+ThXe) - 0.3;
    if(Debug>1) cout << dTecGeom_Radius[i] << " " << TecGeometryObject::get_Radius(i) << endl;
    if(Debug>1) cout << dTecGeom_WireRadius[i] << " " << TecGeometryObject::get_WireRadius(i) << endl;
  }
  if(Debug>1) cout << endl;
  for(i=0; i<TECMAXPLANE; i++) {
    dTecGeom_Zwidth[i] = Zsz0[i];
    if(Debug>1) cout << dTecGeom_Zwidth[i] << " " << TecGeometryObject::get_Zwidth(i) << endl;
  }

  // Fill wire positions relative to the center of each Tec chamber
  for (isect=0; isect<TECMAXSECT; isect++) 
    {
      firstplane=0; lastplane=TECMAXPLANE-1;
      for (iplane=firstplane; iplane<lastplane+1; iplane++) 
	{
	  rasst0 = dTecGeom_WireRadius[iplane];
	  halfang=(dTecGeom_phitopw - dTecGeom_phibotw) / 
	    dTecGeom_sectperarm /2.0 * M_PI / 180.0;
	  rasst0=rasst0/cos(halfang);
	  MidWire = TecGeometryObject::get_NumWires(iplane) / 2.0;
	  for (izsign=0; izsign<TECMAXSIDE; izsign++) 
	    {
	      index=isect*TECMAXPLANE*TECMAXSIDE+iplane*TECMAXSIDE+izsign;
	      for(iwire=0; iwire < TecGeometryObject::get_NumWires(iplane); iwire++) 
		{
		  rwire= iwire;
		  Xwire = 0.0;
		  Ywire = TecGeometryObject::get_WireSpacing(iplane)*(rwire - MidWire - 0.5);
		  if (izsign == 0)
		    {
		      Zwire1 = -dTecGeom_Zwidth[iplane] / 2.0;
		      Zwire2 = 0.0;
		    }
		  else if (izsign == 1)
		    {
		      Zwire1 = 0.0;
		      Zwire2 = dTecGeom_Zwidth[iplane] / 2.0;
		    }
		  else
		    {
		      cout << __FILE__ << ":" << __LINE__
			   << ": Illegal izsign value! Setting izsign = 0!" << endl;
		      Zwire1 = -dTecGeom_Zwidth[iplane] / 2.0;
		      Zwire2 = 0.0;
		    }
		  SouthCoordinate[index][iwire].setX(Xwire);
		  SouthCoordinate[index][iwire].setY(Ywire);
		  NorthCoordinate[index][iwire].setX(Xwire);
		  NorthCoordinate[index][iwire].setY(Ywire);
		  SouthCoordinate[index][iwire].setZ(Zwire1);
		  NorthCoordinate[index][iwire].setZ(Zwire2);
		} // end loop over wires
	    } // end loop over z-sides
        } // end loop over planes
    } // end loop over sectors

  // Fill chamber positions

    float xx1,xx2,yy1,yy2,zz1,zz2;

    for (isect=0; isect<TECMAXSECT; isect++) {
      firstplane=0; lastplane=TECMAXPLANE-1;
      for (iplane=firstplane; iplane<lastplane+1; iplane++) {
        for (izsign=0; izsign<TECMAXSIDE; izsign++) {

          index=isect*TECMAXPLANE*TECMAXSIDE+iplane*TECMAXSIDE+izsign;

          dphi = (dTecGeom_phitope -
                  dTecGeom_phibote)/
                 dTecGeom_sectperarm * M_PI / 180.0;
          phi1 = dTecGeom_phibote* M_PI / 180.0 +
                      isect*dphi;
          phi2 = dTecGeom_phibote* M_PI / 180.0 +
                       (isect+1)*dphi;
          halfang=(dTecGeom_phitopw- dTecGeom_phibotw) /
                  dTecGeom_sectperarm / 2.0* M_PI / 180.0;
          rasst0 = dTecGeom_WireRadius[iplane]/cos(halfang);
          xx1 = rasst0*cos(phi1);
          xx2 = rasst0*cos(phi2);
          yy1 = rasst0*sin(phi1);
          yy2 = rasst0*sin(phi2);
          if(izsign==0) {
            zz1 = -dTecGeom_Zwidth[iplane]/2.;
            zz2 = 0.;
          }
          else {
            zz1 = 0.;
            zz2 = dTecGeom_Zwidth[iplane]/2.;
          }

// Correct (in memory) order of points in panel is:
// 1. Top South
// 2. Bottom South
// 3. Top North

          PHPoint p1(xx1,yy1,zz1); // bottom south
          PHPoint p2(xx2,yy2,zz1); // top south
          PHPoint p3(xx1,yy1,zz2); // bottom north
          PHPoint p4(xx2,yy2,zz2); // top north
          PHPanel pp(p2,p1,p4);
          TecPlane[index] = pp;

          } // end loop over z-sides
        } // end loop over planes
      } // end loop over sectors

  iFlag=0;

// Calculate global coordinates of wires
     for(int index=0; index<TECMAXINDEX; index++) {
       int sector = index/(TECMAXPLANE*TECMAXSIDE);
       int plane = (index - sector*TECMAXPLANE*TECMAXSIDE)/TECMAXSIDE;
       for(int i=0; i<TecGeometryObject::get_NumWires(plane); i++) {
           GlobalX[index][i] = calculateGlobalX(index,i);
           GlobalY[index][i] = calculateGlobalY(index,i);
       }
     }

  return True;
}

/// Calculate default Geant geometry 
PHBoolean 
TecGeometryObject::FetchFromFile() 
{
  int index,isect,iplane,izsign,iwire;
  float dphi,rasst0,MidWire;
  float rwire;
  int firstplane,lastplane;
  double Xpos,Ypos;
  double xx1,xx2,yy1,yy2,zz1,zz2;
  
  firstplane=0; lastplane=TECMAXPLANE-1;

    for (isect=0; isect<TECMAXSECT; isect++) /* 0 = bottom, 3 = top */
    {

      for (iplane=firstplane; iplane<lastplane+1; iplane++)
      {
        rasst0 = TecGeometryObject::get_WireRadius(iplane);
        MidWire = TecGeometryObject::get_NumWires(iplane) / 2.0;

        for (izsign=0; izsign<TECMAXSIDE; izsign++) /* 0=north, 1=south */
        {

          index=isect*TECMAXPLANE*TECMAXSIDE+iplane*TECMAXSIDE+izsign;

          for (iwire=0; iwire<TecGeometryObject::get_NumWires(iplane); iwire++)
          {
            rwire= iwire;
            Xpos = 0.0;
	    // This was wrong. Corrected 11/26/2000. S.L.
            Ypos = TecGeometryObject::get_WireSpacing(iplane)*(rwire - MidWire - 0.5);
	    
	    SouthCoordinate[index][iwire].setX(Xpos);
	    SouthCoordinate[index][iwire].setY(Ypos);
	    NorthCoordinate[index][iwire].setX(Xpos);
	    NorthCoordinate[index][iwire].setY(Ypos);
	    SouthCoordinate[index][iwire].setZ(-TecGeometryObject::get_Zwidth(iplane)/4.0);
	    NorthCoordinate[index][iwire].setZ(TecGeometryObject::get_Zwidth(iplane)/4.0);
          } /*iwire */
	  
	  // Define Tec Planes
	  dphi = (TecGeometryObject::get_phitope() -
		  TecGeometryObject::get_phibote())/4.* M_PI/180.;
          float phi1 = TecGeometryObject::get_phibote()* M_PI/180. + 
                         isect*dphi;
          float phi2 = TecGeometryObject::get_phibote()* M_PI/180. + 
                         (isect+1)*dphi;
	  float halfang=(TecGeometryObject::get_phitopw() - 
			 TecGeometryObject::get_phibotw())
	    / TecGeometryObject::get_sectperarm() 
	    / 2.0 * M_PI / 180.0;
	  
	  xx1 = rasst0*cos(phi1)/cos(halfang);
	  xx2 = rasst0*cos(phi2)/cos(halfang);
	  yy1 = rasst0*sin(phi1)/cos(halfang);
	  yy2 = rasst0*sin(phi2)/cos(halfang);
          if(izsign==0) {
            zz1 = -TecGeometryObject::get_Zwidth(iplane)/2.;
            zz2 = 0.;
          }
          else {
            zz1 = 0.;
            zz2 = TecGeometryObject::get_Zwidth(iplane)/2.;
          }

// Correct (in memory) order of points in panel is:
// 1. Top South
// 2. Bottom South
// 3. Top North

          PHPoint p1(xx1,yy1,zz1); // bottom south
          PHPoint p2(xx2,yy2,zz1); // top south
          PHPoint p3(xx1,yy1,zz2); // bottom north
          PHPoint p4(xx2,yy2,zz2); // top north
	  PHPanel pp(p2,p1,p4);
          TecPlane[index] = pp;

        } /* izsign */

      } /* iplane */

    } /* isect */

  iFlag=0;

// Calculate global coordinates of wires
     for(int index=0; index<TECMAXINDEX; index++) {
       int sector = index/(TECMAXPLANE*TECMAXSIDE);
       int plane = (index - sector*TECMAXPLANE*TECMAXSIDE)/TECMAXSIDE;
       for(int i=0; i<TecGeometryObject::get_NumWires(plane); i++) {
           GlobalX[index][i] = calculateGlobalX(index,i);
           GlobalY[index][i] = calculateGlobalY(index,i);
       }
     }

  return True;
}

/// Fetch Sector Frames from a Database
PHBoolean 
TecGeometryObject::FetchSectorFrames() 
{

PdbCoordinate* achan=0;
int bankid=0;
const char* calibname=CalibName;

  PHTimeStamp tSearch = Tsearch;
  PdbBankID bankID;

// Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;

  if (application->startRead()) {

// Fetch corresponding bank
      bankid = BankNumber + TECMAXINDEX + 2;
      bankID.setInternalValue(bankid);

      tecBank = bankManager->fetchBank("PdbCoordinateBank", bankID, calibname, tSearch);

      if(tecBank) {
        if(Debug>1) tecBank->print();
        if(Debug>1) cout << "Bank Length = " << tecBank->getLength() << endl;

// Read Sector Frames
        for(int i=0; i<TECMAXSECT; i++) {

          achan = (PdbCoordinate*)&(tecBank->getEntry(0+i*4));
          double cenx = achan->getParameter(0);
          double ceny = achan->getParameter(1);
          double cenz = achan->getParameter(2);
          PHPoint center = PHPoint(cenx, ceny, cenz);

          achan = (PdbCoordinate*)&(tecBank->getEntry(1+i*4));
          double Xx = achan->getParameter(0);
          double Xy = achan->getParameter(1);
          double Xz = achan->getParameter(2);
          PHVector xaxisrot = PHVector(Xx, Xy, Xz);

          achan = (PdbCoordinate*)&(tecBank->getEntry(2+i*4));
          double Yx = achan->getParameter(0);
          double Yy = achan->getParameter(1);
          double Yz = achan->getParameter(2);
          PHVector yaxisrot = PHVector(Yx, Yy, Yz);

          achan = (PdbCoordinate*)&(tecBank->getEntry(3+i*4));
          double Zx = achan->getParameter(0);
          double Zy = achan->getParameter(1);
          double Zz = achan->getParameter(2);
          PHVector zaxisrot = PHVector(Zx, Zy, Zz);

	    PHFrame NewFrame = PHFrame(center,xaxisrot,yaxisrot,zaxisrot);
            TecSectorFrame[i] = NewFrame;

        }

// Read Plane Frames
        for(int i=TECMAXSECT; i<TECMAXSECT+TECMAXINDEX; i++) {

          achan = (PdbCoordinate*)&(tecBank->getEntry(0+i*4));
          double cenx = achan->getParameter(0);
          double ceny = achan->getParameter(1);
          double cenz = achan->getParameter(2);
          PHPoint center = PHPoint(cenx, ceny, cenz);

          achan = (PdbCoordinate*)&(tecBank->getEntry(1+i*4));
          double Xx = achan->getParameter(0);
          double Xy = achan->getParameter(1);
          double Xz = achan->getParameter(2);
          PHVector xaxisrot = PHVector(Xx, Xy, Xz);

          achan = (PdbCoordinate*)&(tecBank->getEntry(2+i*4));
          double Yx = achan->getParameter(0);
          double Yy = achan->getParameter(1);
          double Yz = achan->getParameter(2);
          PHVector yaxisrot = PHVector(Yx, Yy, Yz);

          achan = (PdbCoordinate*)&(tecBank->getEntry(3+i*4));
          double Zx = achan->getParameter(0);
          double Zy = achan->getParameter(1);
          double Zz = achan->getParameter(2);
          PHVector zaxisrot = PHVector(Zx, Zy, Zz);

            PHFrame NewFrame = PHFrame(center,xaxisrot,yaxisrot,zaxisrot);
            TecPlaneFrame[i-TECMAXSECT] = NewFrame;
        }

// Read ARM Frame
        for(int i=TECMAXSECT+TECMAXINDEX; i<TECMAXSECT+TECMAXINDEX+1; i++) {

          achan = (PdbCoordinate*)&(tecBank->getEntry(0+i*4));
          double cenx = achan->getParameter(0);
          double ceny = achan->getParameter(1);
          double cenz = achan->getParameter(2);
          PHPoint center = PHPoint(cenx, ceny, cenz);

          achan = (PdbCoordinate*)&(tecBank->getEntry(1+i*4));
          double Xx = achan->getParameter(0);
          double Xy = achan->getParameter(1);
          double Xz = achan->getParameter(2);
          PHVector xaxisrot = PHVector(Xx, Xy, Xz);

          achan = (PdbCoordinate*)&(tecBank->getEntry(2+i*4));
          double Yx = achan->getParameter(0);
          double Yy = achan->getParameter(1);
          double Yz = achan->getParameter(2);
          PHVector yaxisrot = PHVector(Yx, Yy, Yz);

          achan = (PdbCoordinate*)&(tecBank->getEntry(3+i*4));
          double Zx = achan->getParameter(0);
          double Zy = achan->getParameter(1);
          double Zz = achan->getParameter(2);
          PHVector zaxisrot = PHVector(Zx, Zy, Zz);

            PHFrame NewFrame = PHFrame(center,xaxisrot,yaxisrot,zaxisrot);
            EastCarriage = NewFrame;

        }


      }
      else {
         cerr << "TecGeometryObject::FetchSectorFrames(): "
         << "WARNING -> bankManager returned zero-pointer." << endl;
         return False;
      }

    application->commit();
  }
  else {
    application->abort();
    cerr << "TecGeometryObject::FetchSectorFrames(): "
         << "WARNING -> Transaction aborted. Database NOT available." << endl;
    return False;
  }

  if(Debug>0) {
    for(int i=0; i<TECMAXSECT; i++) {
      cout << "Fetched Sector Frame # " << i << endl;
      cout << ((TecSectorFrame[i]).getOrigin()).getX() << " "
           << ((TecSectorFrame[i]).getOrigin()).getY() << " "
           << ((TecSectorFrame[i]).getOrigin()).getZ() << endl;
      cout << ((TecSectorFrame[i]).getU()).getX() << " "
           << ((TecSectorFrame[i]).getU()).getY() << " "
           << ((TecSectorFrame[i]).getU()).getZ() << endl;
      cout << ((TecSectorFrame[i]).getV()).getX() << " "
           << ((TecSectorFrame[i]).getV()).getY() << " "
           << ((TecSectorFrame[i]).getV()).getZ() << endl;
      cout << ((TecSectorFrame[i]).getW()).getX() << " "
           << ((TecSectorFrame[i]).getW()).getY() << " "
           << ((TecSectorFrame[i]).getW()).getZ() << endl;
    }
    for(int i=0; i<TECMAXINDEX; i++) {
      cout << "Fetched Plane Frame # " << i << endl;
      cout << ((TecPlaneFrame[i]).getOrigin()).getX() << " "
           << ((TecPlaneFrame[i]).getOrigin()).getY() << " "
           << ((TecPlaneFrame[i]).getOrigin()).getZ() << endl;
      cout << ((TecPlaneFrame[i]).getU()).getX() << " "
           << ((TecPlaneFrame[i]).getU()).getY() << " "
           << ((TecPlaneFrame[i]).getU()).getZ() << endl;
      cout << ((TecPlaneFrame[i]).getV()).getX() << " "
           << ((TecPlaneFrame[i]).getV()).getY() << " "
           << ((TecPlaneFrame[i]).getV()).getZ() << endl;
      cout << ((TecPlaneFrame[i]).getW()).getX() << " "
           << ((TecPlaneFrame[i]).getW()).getY() << " "
           << ((TecPlaneFrame[i]).getW()).getZ() << endl;
    }
      cout << "Fetched East Arm Frame: " << endl;
      cout << ((EastCarriage).getOrigin()).getX() << " "
           << ((EastCarriage).getOrigin()).getY() << " "
           << ((EastCarriage).getOrigin()).getZ() << endl;
      cout << ((EastCarriage).getU()).getX() << " "
           << ((EastCarriage).getU()).getY() << " "
           << ((EastCarriage).getU()).getZ() << endl;
      cout << ((EastCarriage).getV()).getX() << " "
           << ((EastCarriage).getV()).getY() << " "
           << ((EastCarriage).getV()).getZ() << endl;
      cout << ((EastCarriage).getW()).getX() << " "
           << ((EastCarriage).getW()).getY() << " "
           << ((EastCarriage).getW()).getZ() << endl;

  } // debug

  if(tecBank) delete tecBank;
  return True;

}

/// Fetch a list of active planes from a Database
PHBoolean 
TecGeometryObject::FetchActivePlanes() 
{
  PdbCoordinate* achan=0;
  int bankid=0;
  const char* calibname = CalibName;

  PHTimeStamp tSearch = Tsearch;
  PdbBankID bankID;

// Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;

  if (application->startRead()) {

// Fetch corresponding bank
      bankid = BankNumber + TECMAXINDEX + 1;
      bankID.setInternalValue(bankid);

      tecBank = bankManager->fetchBank("PdbCoordinateBank", bankID, calibname, tSearch);

      if(tecBank) {
        if(Debug>1) tecBank->print();
        if(Debug>1) cout << "Bank Length = " << tecBank->getLength() << endl;

        for(int i=0; i < TECMAXINDEX; i++) {
          achan = (PdbCoordinate*)&(tecBank->getEntry(i));
          ActivePlane[i]=(int)achan->getParameter(0); 
        }
        for(int i=TECMAXINDEX; i < TECMAXINDEX+TECMAXSECT*TECMAXSIDE; i++) {
          achan = (PdbCoordinate*)&(tecBank->getEntry(i));
          ActiveSectorSide[i-TECMAXINDEX]=(int)achan->getParameter(0); 
        }

	  if(Debug>0) {
            for(int i=0; i<TECMAXINDEX; i++) { 
              cout << "ActivePlane: " << i << " " << ActivePlane[i] << endl;
            }
            for(int i=0; i<TECMAXSECT*TECMAXSIDE; i++) {
              cout << "ActiveSectorSide: " << i << " " 
                   << ActiveSectorSide[i] << endl;
            }
          }

      }
      else {
         cerr << "TecGeometryObject::FetchActivePlanes(): "
         << "ERROR -> bankManager returned zero-pointer." << endl;
         return False;
      }

    application->commit();
  }
  else {
    application->abort();
    cerr << "TecGeometryObject::FetchActivePlanes(): "
         << "ERROR -> Transaction aborted. Database NOT available." << endl;
    return False;
  }
 
  if(tecBank) delete tecBank;
  return True;
}

/// Fetch geometry information for this object from a Database
PHBoolean
TecGeometryObject::Fetch()
{
  PdbCoordinate* achan=0;
  int index, i, bankid, chamber, point;
  float X1, Y1, Z1;
  const char* calibname = CalibName;

  PHTimeStamp tSearch = Tsearch;
  PdbBankID bankID;

  // Open database in readonly mode
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;

  if (!application->startRead())
    {
      application->abort();
      cerr << "TecGeometryObject::Fetch(): "
	   << "ERROR -> Transaction aborted. Database NOT available." << endl;
      return False;
    }

  // Loop over chambers
  for (index = 0; index < TECMAXINDEX; index++) {

      // Fetch corresponding bank
      bankid = BankNumber + index;
      bankID.setInternalValue(bankid);
      
      tecBank = bankManager->fetchBank("PdbCoordinateBank", bankID, calibname, tSearch);
      
      if (tecBank) {

	  for (i = 0; i < (int)tecBank->getLength(); i++)
	    {
	      achan = (PdbCoordinate*) & (tecBank->getEntry(i));
	      // South Coordinates
	      SouthCoordinate[index][i].setX(achan->getParameter(0));
	      SouthCoordinate[index][i].setY(achan->getParameter(1));
	      SouthCoordinate[index][i].setZ(achan->getParameter(2));
	      
	      // North Coordinates
	      NorthCoordinate[index][i].setX(achan->getParError(0));
	      NorthCoordinate[index][i].setY(achan->getParError(1));
	      NorthCoordinate[index][i].setZ(achan->getParError(2));
	    }
        delete tecBank;
      }
      else {
	  cerr << "TecGeometryObject::Fetch(): "
	       << "ERROR -> bankManager returned zero-pointer." << endl;
      }
  }
  
  // Read chambers positions
  PHPoint p1, p2, p3, p4;

  bankid = BankNumber + TECMAXINDEX;
  bankID.setInternalValue(bankid);

  tecBank = bankManager->fetchBank("PdbCoordinateBank", bankID, calibname, tSearch);
  if (tecBank)
    {
      for (i = 0; i < (int)tecBank->getLength(); i++)
	{
	  achan = (PdbCoordinate*) & (tecBank->getEntry(i));
	  X1 = achan->getParameter(0);
	  Y1 = achan->getParameter(1);
	  Z1 = achan->getParameter(2);
	  chamber = i / 4;
	  point = i % 4;

// Correct (in memory) order of points in panel is:
// 1. Top South
// 2. Bottom South
// 3. Top North
// Actuall order of points in database and ascii files is:
// 1. Bottom South
// 2. Top South
// 3. Top North

	  if (point == 0)
	    {
	      p1.setX(X1);
	      p1.setY(Y1);
	      p1.setZ(Z1);
	    }
	  if (point == 1)
	    {
	      p2.setX(X1);
	      p2.setY(Y1);
	      p2.setZ(Z1);
	    }
	  if (point == 2)
	    {
	      p3.setX(X1);
	      p3.setY(Y1);
	      p3.setZ(Z1);
	    }
	  if (point == 3)
	    {
	      p4.setX(X1);
	      p4.setY(Y1);
	      p4.setZ(Z1);
	    }
	  if (point == 2)
	    {
	      PHPanel pp(p2, p1, p3);
	      TecPlane[chamber] = pp;
	    }

	}
    }
  else
    {
      cerr << "TecGeometryObject::Fetch(): "
	   << "ERROR -> bankManager returned zero-pointer." << endl;
    }
  
  application->commit();
 
  iFlag = 0;

        if(Debug>1) {
         for(int index=0; index<48; index++) {
          float xx1 = ((TecPlane[index]).getPoint(0)).getX();
          float xx2 = ((TecPlane[index]).getPoint(1)).getX();
          float xx3 = ((TecPlane[index]).getPoint(2)).getX();
          float xx4 = ((TecPlane[index]).getPoint(3)).getX();
          float yy1 = ((TecPlane[index]).getPoint(0)).getY();
          float yy2 = ((TecPlane[index]).getPoint(1)).getY();
          float yy3 = ((TecPlane[index]).getPoint(2)).getY();
          float yy4 = ((TecPlane[index]).getPoint(3)).getY();
          float zz1 = ((TecPlane[index]).getPoint(0)).getZ();
          float zz2 = ((TecPlane[index]).getPoint(1)).getZ();
          float zz3 = ((TecPlane[index]).getPoint(2)).getZ();
          float zz4 = ((TecPlane[index]).getPoint(3)).getZ();
          cout << "index = " << index << " " << index/12 << endl;
          cout << " 1 " << xx1 << " " << yy1 << " " << zz1 << endl;
          cout << " 2 " << xx2 << " " << yy2 << " " << zz2 << endl;
          cout << " 3 " << xx3 << " " << yy3 << " " << zz3 << endl;
          cout << " 4 " << xx4 << " " << yy4 << " " << zz4 << endl;
         }
        }

  // Fetch Sector Frames
  PHBoolean statussf = FetchSectorFrames();
  if (!statussf)
    {
      cerr << "TecGeometryObject::Fetch() WARNING: "
	   << "Failed to fetch Sector Frames. Default will be used." << endl;
    }

  // Calculate global coordinates of wires
  for (int index = 0; index < TECMAXINDEX; index++)
    {
      int sector = index / (TECMAXPLANE * TECMAXSIDE);
      int plane = (index - sector * TECMAXPLANE * TECMAXSIDE) / TECMAXSIDE;
      for (int i = 0; i < TecGeometryObject::get_NumWires(plane); i++)
        {
          GlobalX[index][i] = calculateGlobalX(index, i);
          GlobalY[index][i] = calculateGlobalY(index, i);
        }
    }

  // Fetch a list of active planes
  PHBoolean statusap = FetchActivePlanes();
  if (!statusap)
    {
      cerr << "TecGeometryObject::Fetch() WARNING: "
	   << "Failed to fetch a list of active planes. Default will be used." << endl;
    }

  if(tecBank) delete tecBank;
  return True;
}

PHBoolean 
TecGeometryObject::Shift(float dx, float dy, float dz) 
{
  PHVector shiftv = PHVector(dx, dy, dz);

  return TecGeometryObject::Shift(&shiftv);
}

PHBoolean 
TecGeometryObject::Shift(PHVector* shiftv) 
{
  PHPoint shiftp = *shiftv;

  int i,j;
  float dx = shiftp.getX();
  float dy = shiftp.getY();

  for(i=0; i<TECMAXINDEX; i++) {

    PHPanel planein = TecPlane[i];
  
    if(Debug>2) {
      cout << "Original TecPlane # " << i << endl;
      for(j=0; j<4; j++) { 
        cout << "point " << j << " "
             << (planein.getPoint(j)).getX() << " " 
             << (planein.getPoint(j)).getY() << " "
             << (planein.getPoint(j)).getZ() << " " << endl;
      }
    }

      PHPoint point1 = planein.getPoint(0) + shiftp;
      PHPoint point2 = planein.getPoint(1) + shiftp;
      PHPoint point3 = planein.getPoint(2) + shiftp;
      PHPanel planeout = PHPanel(point1, point2, point3);
      TecPlane[i]=planeout;

    if(Debug>2) { 
      cout << "After shift TecPlane # " << i << endl;
      for(j=0; j<4; j++) {
        cout << "point " << j << " "
             << (planeout.getPoint(j)).getX() << " " 
             << (planeout.getPoint(j)).getY() << " "
             << (planeout.getPoint(j)).getZ() << " " << endl;
      }
    }

// Shift global coordinates as well
    int sector = i/(TECMAXPLANE*TECMAXSIDE);
    int plane = (i - sector*(TECMAXPLANE*TECMAXSIDE))/TECMAXSIDE;
    for(int j=0; j<TecGeometryObject::get_NumWires(plane); j++) {
      GlobalX[i][j] += dx;
      GlobalY[i][j] += dy;
    }

  } // end i loop over tec planes

  return True;

}

PHBoolean 
TecGeometryObject::UpdateSectorFrames(PHTimeStamp* Tbeg, 
				      PHTimeStamp* Tend) 
{
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank = 0;
  
  // Open the federation in update mode
   if(Debug>0) cout << "TecGeometryObject::UpdateActivePlanes: "
               << "Opening FD in update mode..." << endl;

   if (application->startUpdate()) {

     PHTimeStamp tStart = *Tbeg;
     PHTimeStamp tStop  = *Tend;
     PdbBankID bankID;
     const char *descrip   = "Sector Frames";
     const char *calibname = CalibName;
     int bankid=0;
     PdbCoordinate* achan=0;

     bankid = TECGEOMBANK + TECMAXINDEX + 2;
     bankID.setInternalValue(bankid);
     if(Debug>0) {
       cout << "TecGeometryObject::UpdateSectorFrames: "
            << "Committing SectorFrames... " << endl;
       cout << "TecAddressObject::UpdateSectorFrames: calibname = " 
            << calibname << endl;
       cout << "TecAddressObject::UpdateSectorFrames: bankid = " 
            << bankid << endl;
       cout << "Start validity = "; tStart.print(); cout << endl;
       cout << "End validity = "; tStop.print(); cout << endl;
     }

     tecBank = bankManager->createBank("PdbCoordinateBank", bankID, descrip, tStart, tStop, calibname);
     tecBank->setLength((TECMAXSECT+TECMAXINDEX+1)*4);
     if(Debug>2) tecBank->print();

// Update Sector Frames
       for(int i=0; i<TECMAXSECT; i++) {

         if(Debug>0) cout << "Updating Sector Frame # " << i << endl;

// center (PHPoint)
         achan = (PdbCoordinate*)&(tecBank->getEntry(0+i*4));
         double cenx = ((TecSectorFrame[i]).getOrigin()).getX();
         double ceny = ((TecSectorFrame[i]).getOrigin()).getY();
         double cenz = ((TecSectorFrame[i]).getOrigin()).getZ();
           achan->setParameter(0, cenx);
           achan->setParameter(1, ceny);
           achan->setParameter(2, cenz);
           achan->setParError(0, 0.);
           achan->setParError(1, 0.);
           achan->setParError(2, 0.);
           if(Debug>0) cout << "Updating: " 
                            << cenx << " " << ceny << " " << cenz << endl;

// X axis (PHVector)
         achan = (PdbCoordinate*)&(tecBank->getEntry(1+i*4));
         double Xx = ((TecSectorFrame[i]).getU()).getX();
         double Xy = ((TecSectorFrame[i]).getU()).getY();
         double Xz = ((TecSectorFrame[i]).getU()).getZ();
           achan->setParameter(0, Xx);
           achan->setParameter(1, Xy);
           achan->setParameter(2, Xz);
           achan->setParError(0, 0.);
           achan->setParError(1, 0.);
           achan->setParError(2, 0.);
           if(Debug>0) cout << "Updating: " 
                            << Xx << " " << Xy << " " << Xz << endl;

// Y axis (PHVector)
         achan = (PdbCoordinate*)&(tecBank->getEntry(2+i*4));
         double Yx = ((TecSectorFrame[i]).getV()).getX();
         double Yy = ((TecSectorFrame[i]).getV()).getY();
         double Yz = ((TecSectorFrame[i]).getV()).getZ();
           achan->setParameter(0, Yx);
           achan->setParameter(1, Yy);
           achan->setParameter(2, Yz);
           achan->setParError(0, 0.);
           achan->setParError(1, 0.);
           achan->setParError(2, 0.);
           if(Debug>0) cout << "Updating: " 
                            << Yx << " " << Yy << " " << Yz << endl;

// Z axis (PHVector)
         achan = (PdbCoordinate*)&(tecBank->getEntry(3+i*4));
         double Zx = ((TecSectorFrame[i]).getW()).getX();
         double Zy = ((TecSectorFrame[i]).getW()).getY();
         double Zz = ((TecSectorFrame[i]).getW()).getZ();
           achan->setParameter(0, Zx);
           achan->setParameter(1, Zy);
           achan->setParameter(2, Zz);
           achan->setParError(0, 0.);
           achan->setParError(1, 0.);
           achan->setParError(2, 0.);
           if(Debug>0) cout << "Updating: " 
                            << Zx << " " << Zy << " " << Zz << endl;

       } // sector frames update

// Update Plane Frames
       for(int i=TECMAXSECT; i<TECMAXSECT+TECMAXINDEX; i++) {

         if(Debug>0) cout << "Updating Plane Frame # " << i-TECMAXSECT << endl;

// center (PHPoint)
         achan = (PdbCoordinate*)&(tecBank->getEntry(0+i*4));
         double cenx = ((TecPlaneFrame[i-TECMAXSECT]).getOrigin()).getX();
         double ceny = ((TecPlaneFrame[i-TECMAXSECT]).getOrigin()).getY();
         double cenz = ((TecPlaneFrame[i-TECMAXSECT]).getOrigin()).getZ();
           achan->setParameter(0, cenx);
           achan->setParameter(1, ceny);
           achan->setParameter(2, cenz);
           achan->setParError(0, 0.);
           achan->setParError(1, 0.);
           achan->setParError(2, 0.);
           if(Debug>0) cout << "Updating: "
                            << cenx << " " << ceny << " " << cenz << endl;

// X axis (PHVector)
         achan = (PdbCoordinate*)&(tecBank->getEntry(1+i*4));
         double Xx = ((TecPlaneFrame[i-TECMAXSECT]).getU()).getX();
         double Xy = ((TecPlaneFrame[i-TECMAXSECT]).getU()).getY();
         double Xz = ((TecPlaneFrame[i-TECMAXSECT]).getU()).getZ();
           achan->setParameter(0, Xx);
           achan->setParameter(1, Xy);
           achan->setParameter(2, Xz);
           achan->setParError(0, 0.);
           achan->setParError(1, 0.);
           achan->setParError(2, 0.);
           if(Debug>0) cout << "Updating: "
                            << Xx << " " << Xy << " " << Xz << endl;

// Y axis (PHVector)
         achan = (PdbCoordinate*)&(tecBank->getEntry(2+i*4));
         double Yx = ((TecPlaneFrame[i-TECMAXSECT]).getV()).getX();
         double Yy = ((TecPlaneFrame[i-TECMAXSECT]).getV()).getY();
         double Yz = ((TecPlaneFrame[i-TECMAXSECT]).getV()).getZ();
           achan->setParameter(0, Yx);
           achan->setParameter(1, Yy);
           achan->setParameter(2, Yz);
           achan->setParError(0, 0.);
           achan->setParError(1, 0.);
           achan->setParError(2, 0.);
           if(Debug>0) cout << "Updating: "
                            << Yx << " " << Yy << " " << Yz << endl;

// Z axis (PHVector)
         achan = (PdbCoordinate*)&(tecBank->getEntry(3+i*4));
         double Zx = ((TecPlaneFrame[i-TECMAXSECT]).getW()).getX();
         double Zy = ((TecPlaneFrame[i-TECMAXSECT]).getW()).getY();
         double Zz = ((TecPlaneFrame[i-TECMAXSECT]).getW()).getZ();
           achan->setParameter(0, Zx);
           achan->setParameter(1, Zy);
           achan->setParameter(2, Zz);
           achan->setParError(0, 0.);
           achan->setParError(1, 0.);
           achan->setParError(2, 0.);
           if(Debug>0) cout << "Updating: "
                            << Zx << " " << Zy << " " << Zz << endl;

       } // plane frames update

// Update Arm Frame
       for(int i=TECMAXSECT+TECMAXINDEX; i<TECMAXSECT+TECMAXINDEX+1; i++) {

         if(Debug>0) cout << "Updating EastArm Frame: " << endl;

// center (PHPoint)
         achan = (PdbCoordinate*)&(tecBank->getEntry(0+i*4));
         double cenx = ((EastCarriage).getOrigin()).getX();
         double ceny = ((EastCarriage).getOrigin()).getY();
         double cenz = ((EastCarriage).getOrigin()).getZ();
           achan->setParameter(0, cenx);
           achan->setParameter(1, ceny);
           achan->setParameter(2, cenz);
           achan->setParError(0, 0.);
           achan->setParError(1, 0.);
           achan->setParError(2, 0.);
           if(Debug>0) cout << "Updating: "
                            << cenx << " " << ceny << " " << cenz << endl;

// X axis (PHVector)
         achan = (PdbCoordinate*)&(tecBank->getEntry(1+i*4));
         double Xx = ((EastCarriage).getU()).getX();
         double Xy = ((EastCarriage).getU()).getY();
         double Xz = ((EastCarriage).getU()).getZ();
           achan->setParameter(0, Xx);
           achan->setParameter(1, Xy);
           achan->setParameter(2, Xz);
           achan->setParError(0, 0.);
           achan->setParError(1, 0.);
           achan->setParError(2, 0.);
           if(Debug>0) cout << "Updating: "
                            << Xx << " " << Xy << " " << Xz << endl;

// Y axis (PHVector)
         achan = (PdbCoordinate*)&(tecBank->getEntry(2+i*4));
         double Yx = ((EastCarriage).getV()).getX();
         double Yy = ((EastCarriage).getV()).getY();
         double Yz = ((EastCarriage).getV()).getZ();
           achan->setParameter(0, Yx);
           achan->setParameter(1, Yy);
           achan->setParameter(2, Yz);
           achan->setParError(0, 0.);
           achan->setParError(1, 0.);
           achan->setParError(2, 0.);
           if(Debug>0) cout << "Updating: "
                            << Yx << " " << Yy << " " << Yz << endl;

// Z axis (PHVector)
         achan = (PdbCoordinate*)&(tecBank->getEntry(3+i*4));
         double Zx = ((EastCarriage).getW()).getX();
         double Zy = ((EastCarriage).getW()).getY();
         double Zz = ((EastCarriage).getW()).getZ();
           achan->setParameter(0, Zx);
           achan->setParameter(1, Zy);
           achan->setParameter(2, Zz);
           achan->setParError(0, 0.);
           achan->setParError(1, 0.);
           achan->setParError(2, 0.);
           if(Debug>0) cout << "Updating: "
                            << Zx << " " << Zy << " " << Zz << endl;

       } // arm frames update

     application->commit();
   }
   else {
     cerr << "TecGeometryObject::UpdateSectorFrames ERROR: "
          << "failed to start application for update." << endl;
     return False;
   }

  if(tecBank) delete tecBank;
  return True;

}

PHBoolean 
TecGeometryObject::UpdateActivePlanes(PHTimeStamp* Tbeg, 
				      PHTimeStamp* Tend) 
{
  if(iFlag!=0) {
    cerr << "TecGeometryObject::UpdateActivePlanes ERROR: TGO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    return False;
  }

   PdbBankManager *bankManager = PdbBankManager::instance();
   PdbApplication *application = bankManager->getApplication();
   PdbCalBank *tecBank = 0;

// Open the federation in update mode
   if(Debug>0) cout << "TecGeometryObject::UpdateActivePlanes: Opening FD in update mode..." << endl;

   if (application->startUpdate()) {

     PHTimeStamp tStart = *Tbeg;
     PHTimeStamp tStop  = *Tend;
     PdbBankID bankID;
     const char *descrip   = "List of Active Planes";
     const char *calibname = CalibName;
     int bankid=0;
       int nok=0;
       PdbCoordinate* achan=0;

     bankid = TECGEOMBANK + TECMAXINDEX + 1;
     bankID.setInternalValue(bankid);
     if(Debug>0) {
       cout << "TecGeometryObject::UpdateActivePlanes: Committing Active Planes... " << endl;
       cout << "TecAddressObject::UpdateActivePlanes: calibname = " << calibname << endl;
       cout << "TecAddressObject::UpdateActivePlanes: bankid = " << bankid << endl;
       cout << "Start validity = "; tStart.print(); cout << endl;
       cout << "End validity = "; tStop.print(); cout << endl;
     }

     tecBank = bankManager->createBank("PdbCoordinateBank", bankID, descrip, tStart, tStop, calibname);
     tecBank->setLength(TECMAXINDEX+TECMAXSECT*TECMAXSIDE);
     if(Debug>2) tecBank->print();

       for(int i=0; i<TECMAXINDEX; i++) {
            achan = (PdbCoordinate*)&(tecBank->getEntry(i));
            float ap = ActivePlane[i];
              achan->setParameter(0, ap);
              achan->setParameter(1, 0.);
              achan->setParameter(2, 0.);
                achan->setParError(0, 0.);
                achan->setParError(1, 0.);
                achan->setParError(2, 0.);
                if(Debug>1) cout << "ActivePlane: " << i << " " << ap << endl;
                  nok++;
       }
       for(int i=TECMAXINDEX; i<TECMAXINDEX+TECMAXSECT*TECMAXSIDE; i++) {
            achan = (PdbCoordinate*)&(tecBank->getEntry(i));
            float ap = ActiveSectorSide[i-TECMAXINDEX];
              achan->setParameter(0, ap);
              achan->setParameter(1, 0.);
              achan->setParameter(2, 0.);
                achan->setParError(0, 0.);
                achan->setParError(1, 0.);
                achan->setParError(2, 0.);
                if(Debug>1) cout << "ActiveSectorSide: " << i-TECMAXINDEX << " " << ap << endl;
                  nok++;
       }

     application->commit();
   }
   else {
     cerr << "TecGeometryObject::UpdateActivePlanes ERROR: "
          << "failed to start application for update." << endl;
     return False;
   }

  if(tecBank) delete tecBank;
  return True;
}

PHBoolean 
TecGeometryObject::UpdatePanels(PHTimeStamp* Tbeg, 
				PHTimeStamp* Tend) 
{

  if(iFlag!=0) {
    cerr << "TecGeometryObject::UpdatePanels ERROR: TGO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    return False;
  }

   PdbBankManager *bankManager = PdbBankManager::instance();
   PdbApplication *application = bankManager->getApplication();
    PdbCalBank *tecBank = 0;

   if(Debug>0) cout << "TecGeometryObject::UpdatePanels: Opening FD in update mode..." << endl;
   if (application->startUpdate()) {

   PHTimeStamp tStart = *Tbeg;
   PHTimeStamp tStop  = *Tend;
   PdbBankID bankID;
   const char *descrip   = Description;
   const char *calibname = CalibName;

// Fill chamber positions

    PdbCoordinate* achan;
    float xx1,xx2,xx3,yy1,yy2,yy3,zz1,zz2,zz3,xx4,yy4,zz4;
    int isect,iplane,izsign,index,bankid,firstplane,lastplane;
    int nok=0;

    bankid = TECGEOMBANK + TECMAXINDEX;
    bankID.setInternalValue(bankid);
    if(Debug>0) {
       cout << "TecGeometryObject::UpdatePanels: Filling chamber positions... " << endl;
       cout << "TecAddressObject::UpdatePanels: calibname = " << calibname << endl;
       cout << "TecAddressObject::UpdatePanels: bankid = " << bankid << endl;
       cout << "Start validity = "; tStart.print(); cout << endl;
       cout << "End validity = "; tStop.print(); cout << endl;
    }

    tecBank = bankManager->createBank("PdbCoordinateBank", bankID, descrip, tStart, tStop, calibname);
    tecBank->setLength(TECMAXINDEX*4);
    if(Debug>2) tecBank->print();

    for (isect=0; isect<TECMAXSECT; isect++) {
      firstplane=0; lastplane=TECMAXPLANE-1;
      for (iplane=firstplane; iplane<lastplane+1; iplane++) {
        for (izsign=0; izsign<TECMAXSIDE; izsign++) {

          index=isect*TECMAXPLANE*TECMAXSIDE+iplane*TECMAXSIDE+izsign;

// Correct (in memory) order of points in panel is:
// 1. Top South
// 2. Bottom South
// 3. Top North
// Actuall order of points in database and ascii files is:
// 1. Bottom South
// 2. Top South
// 3. Top North
// Must invert first and second points to keep this.

          xx1 = ((TecPlane[index]).getPoint(0)).getX();
          xx2 = ((TecPlane[index]).getPoint(1)).getX();
          xx3 = ((TecPlane[index]).getPoint(2)).getX();
          xx4 = ((TecPlane[index]).getPoint(3)).getX();
          yy1 = ((TecPlane[index]).getPoint(0)).getY();
          yy2 = ((TecPlane[index]).getPoint(1)).getY();
          yy3 = ((TecPlane[index]).getPoint(2)).getY();
          yy4 = ((TecPlane[index]).getPoint(3)).getY();
          zz1 = ((TecPlane[index]).getPoint(0)).getZ();
          zz2 = ((TecPlane[index]).getPoint(1)).getZ();
          zz3 = ((TecPlane[index]).getPoint(2)).getZ();
          zz4 = ((TecPlane[index]).getPoint(3)).getZ();


          if(Debug>1) {
             cout << "Updating panels for index: " <<  index << " " << index/12 << endl;
             cout << " -1- " << xx1 << " " << yy1 << " " << zz1 << endl;
             cout << " -2- " << xx2 << " " << yy2 << " " << zz2 << endl;
             cout << " -3- " << xx3 << " " << yy3 << " " << zz3 << endl;
             cout << " -4- " << xx4 << " " << yy4 << " " << zz4 << endl;
           }

            achan = (PdbCoordinate*)&(tecBank->getEntry(nok));
              achan->setParameter(0, xx2);
              achan->setParameter(1, yy2);
              achan->setParameter(2, zz2);
                achan->setParError(0, 0.);
                achan->setParError(1, 0.);
                achan->setParError(2, 0.);
                  nok++;
            achan = (PdbCoordinate*)&(tecBank->getEntry(nok));
              achan->setParameter(0, xx1);
              achan->setParameter(1, yy1);
              achan->setParameter(2, zz1);
                achan->setParError(0, 0.);
                achan->setParError(1, 0.);
                achan->setParError(2, 0.);
                  nok++;
            achan = (PdbCoordinate*)&(tecBank->getEntry(nok));
              achan->setParameter(0, xx3);
              achan->setParameter(1, yy3);
              achan->setParameter(2, zz3);
                achan->setParError(0, 0.);
                achan->setParError(1, 0.);
                achan->setParError(2, 0.);
                  nok++;
            achan = (PdbCoordinate*)&(tecBank->getEntry(nok));
              achan->setParameter(0, xx3-xx2+xx1);
              achan->setParameter(1, yy3-yy2+yy1);
              achan->setParameter(2, zz3-zz2+zz1);
                achan->setParError(0, 0.);
                achan->setParError(1, 0.);
                achan->setParError(2, 0.);
                  nok++;

          } // end loop over z-sides
        } // end loop over planes
      } // end loop over sectors

      application->commit();
   }
   else {
     cerr << "TecGeometryObject::UpdatePanels ERROR: failed to start application for update." << endl;
     return False;
   }

  if(tecBank) delete tecBank;
  return True;

}

PHBoolean 
TecGeometryObject::Update(PHTimeStamp* Tbeg, 
			  PHTimeStamp* Tend) 
{

  if(iFlag!=0) {
    cerr << "TecGeometryObject::Update ERROR: TGO not initialized." << endl
         << "    Use Fetch() or FetchFromFile() methods first." << endl;
    return False;
  }

   PdbBankManager *bankManager = PdbBankManager::instance();
   PdbApplication *application = bankManager->getApplication();
   PdbCalBank *tecBank = 0;

   if(Debug>0) cout << "TecGeometryObject::Update: Opening FD in update mode..." << endl;
   if (application->startUpdate()) {
     PHTimeStamp tStart = *Tbeg;
     PHTimeStamp tStop  = *Tend;
     PdbBankID bankID;
     const char *descrip   = Description;
     const char *calibname = CalibName;

     if(Debug>0) {
       cout << "TecAddressObject::Update: calibname = " << calibname << endl;
       cout << "Start validity = "; tStart.print(); cout << endl;
       cout << "End validity = "; tStop.print(); cout << endl;
     }

// Fill wire positions relative to the center of each Tec chamber

  int isect,iplane,izsign,index,bankid,firstplane,lastplane;
  unsigned int iwire;

  PdbCoordinate* achan=0;

    for (isect=0; isect<TECMAXSECT; isect++) {

      firstplane=0; lastplane=TECMAXPLANE-1;
      for (iplane=firstplane; iplane<lastplane+1; iplane++) {

        for (izsign=0; izsign<TECMAXSIDE; izsign++) {

          index=isect*TECMAXPLANE*TECMAXSIDE+iplane*TECMAXSIDE+izsign;

            bankid = TECGEOMBANK+index;

            bankID.setInternalValue(bankid);

            if(Debug>0) cout << "Filling database for Chamber # " << index << endl;

            tecBank = bankManager->createBank("PdbCoordinateBank", bankID, descrip, tStart, tStop, calibname);

            if(tecBank) {

              int itmp = TecGeometryObject::get_NumWires(iplane);
              tecBank->setLength(itmp);

              if(Debug>2) tecBank->print();

              for(iwire=0; iwire < tecBank->getLength(); iwire++) {

                float Xwire1 = (SouthCoordinate[index][iwire]).getX();
                float Xwire2 = (NorthCoordinate[index][iwire]).getX();
                float Ywire1 = (SouthCoordinate[index][iwire]).getY();
                float Ywire2 = (NorthCoordinate[index][iwire]).getY();
                float Zwire1 = (SouthCoordinate[index][iwire]).getZ();
                float Zwire2 = (NorthCoordinate[index][iwire]).getZ(); 

                achan = (PdbCoordinate*)&(tecBank->getEntry(iwire));
// South Coordinates
                achan->setParameter(0, Xwire1);
                achan->setParameter(1, Ywire1);
                achan->setParameter(2, Zwire1);
// North Coordinates
                achan->setParError(0, Xwire2);
                achan->setParError(1, Ywire2);
                achan->setParError(2, Zwire2);
                if(Debug>5) tecBank->printEntry(iwire);
              } // end loop over wires
              
              delete tecBank;
            } // tecBank != 0

          } // end loop over z-sides
        } // end loop over planes
      } // end loop over sectors

// Fill chamber positions

    float xx1,xx2,xx3,yy1,yy2,yy3,zz1,zz2,zz3,xx4,yy4,zz4;
    int nok=0;

    bankid = TECGEOMBANK + TECMAXINDEX;
    bankID.setInternalValue(bankid);
    if(Debug>0) {
       cout << "TecGeometryObject::Update: Filling chamber positions... " << endl;
       cout << "TecAddressObject::Update: calibname = " << calibname << endl;
       cout << "TecAddressObject::Update: bankid = " << bankid << endl;
       cout << "Start validity = "; tStart.print(); cout << endl;
       cout << "End validity = "; tStop.print(); cout << endl;
    }

    tecBank = bankManager->createBank("PdbCoordinateBank", bankID, descrip, tStart, tStop, calibname);
    tecBank->setLength(TECMAXINDEX*4);
    if(Debug>2) tecBank->print();

    for (isect=0; isect<TECMAXSECT; isect++) {
      firstplane=0; lastplane=TECMAXPLANE-1;
      for (iplane=firstplane; iplane<lastplane+1; iplane++) {
        for (izsign=0; izsign<TECMAXSIDE; izsign++) {

          index=isect*TECMAXPLANE*TECMAXSIDE+iplane*TECMAXSIDE+izsign;

// Correct (in memory) order of points in panel is:
// 1. Top South
// 2. Bottom South
// 3. Top North
// Actuall order of points in database and ascii files is:
// 1. Bottom South
// 2. Top South
// 3. Top North
// Must exchange first and second points to keep this

          xx1 = ((TecPlane[index]).getPoint(0)).getX();
          xx2 = ((TecPlane[index]).getPoint(1)).getX();
          xx3 = ((TecPlane[index]).getPoint(2)).getX();
          xx4 = ((TecPlane[index]).getPoint(3)).getX();
          yy1 = ((TecPlane[index]).getPoint(0)).getY();
          yy2 = ((TecPlane[index]).getPoint(1)).getY();
          yy3 = ((TecPlane[index]).getPoint(2)).getY();
          yy4 = ((TecPlane[index]).getPoint(3)).getY();
          zz1 = ((TecPlane[index]).getPoint(0)).getZ();
          zz2 = ((TecPlane[index]).getPoint(1)).getZ();
          zz3 = ((TecPlane[index]).getPoint(2)).getZ();
          zz4 = ((TecPlane[index]).getPoint(3)).getZ();

          if(Debug>1) {
             cout << "Updating panels for index: " <<  index << " " << index/12 << endl;
             cout << " -1- " << xx1 << " " << yy1 << " " << zz1 << endl;
             cout << " -2- " << xx2 << " " << yy2 << " " << zz2 << endl;
             cout << " -3- " << xx3 << " " << yy3 << " " << zz3 << endl;
             cout << " -4- " << xx4 << " " << yy4 << " " << zz4 << endl;
           }

            achan = (PdbCoordinate*)&(tecBank->getEntry(nok));
              achan->setParameter(0, xx2);
              achan->setParameter(1, yy2);
              achan->setParameter(2, zz2);
                achan->setParError(0, 0.);
                achan->setParError(1, 0.);
                achan->setParError(2, 0.);
                  nok++;
            achan = (PdbCoordinate*)&(tecBank->getEntry(nok));
              achan->setParameter(0, xx1);
              achan->setParameter(1, yy1);
              achan->setParameter(2, zz1);
                achan->setParError(0, 0.);
                achan->setParError(1, 0.);
                achan->setParError(2, 0.);
                  nok++;
            achan = (PdbCoordinate*)&(tecBank->getEntry(nok));
              achan->setParameter(0, xx3);
              achan->setParameter(1, yy3);
              achan->setParameter(2, zz3);
                achan->setParError(0, 0.);
                achan->setParError(1, 0.);
                achan->setParError(2, 0.);
                  nok++;
            achan = (PdbCoordinate*)&(tecBank->getEntry(nok));
              achan->setParameter(0, xx3-xx2+xx1);
              achan->setParameter(1, yy3-yy2+yy1);
              achan->setParameter(2, zz3-zz2+zz1);
                achan->setParError(0, 0.);
                achan->setParError(1, 0.);
                achan->setParError(2, 0.);
                  nok++;

          } // end loop over z-sides
        } // end loop over planes
      } // end loop over sectors

// Commit Active Planes

     PHBoolean statusap = UpdateActivePlanes(&tStart, &tStop);
       if(!statusap) {
         cerr << "TecGeometryObject::Update ERROR: " 
              << "Failed to update the list of Active Planes." << endl;
       }

// Commit Sector Frames

     PHBoolean statussf = UpdateSectorFrames(&tStart, &tStop);
       if(!statussf) {
         cerr << "TecGeometryObject::Update ERROR: "
              << "Failed to update Sector/Plane Frames." << endl;
       }


     application->commit();
   }
   else {
     cerr << "TecGeometryObject::Update ERROR: "
          << "Failed to start application for update." << endl;
     return False;
   }

  if(tecBank) delete tecBank;
  return True;

}

PHBoolean TecGeometryObject::UpdateMisalignment(PHTimeStamp* Tbeg, PHTimeStamp* Tend) {

// Fetch current wire positions

  setTimeStamp((*Tbeg)+30); // +30 sec
  if(!Fetch()) {
    cerr << __FILE__ << ":" << __LINE__ << " ";
    cerr << "ERROR Fetching Tec geometry from the Database." << endl;
    return False;
  }

// Read misalignment from file

  if(!ReadMisalignmentFromFile("tecgeom_misalignment_run03.txt")) {
    return False;
  }

// Update geometry object in the database

  if(!Update(Tbeg, Tend)) {
    cerr << __FILE__ << ":" << __LINE__ << " ";
    cerr << "ERROR Failed to update Tec geometry in the Database." << endl;
    return False;
  }

  return True;
}

PHBoolean TecGeometryObject::ReadMisalignmentFromFile(const char *misfile) {

  FILE *file= fopen(misfile,"r");
  if(!file) {
    cerr << __FILE__ << ":" << __LINE__ << " ";
    cerr << "ERROR opening file " << misfile << endl;
    return False;
  }
  float misalign;
  int ndata,index,iwire;
  char head,tail;
  while(fscanf(file,"%c%d,%d,%d,%f%c\n",&head,&index,&iwire,&ndata,&misalign,&tail)!=EOF)
    {
           if(head!='(' || tail!=')') {
             cerr << __FILE__ << ":" << __LINE__ << " ";
             cerr << "ERROR: " << misfile << " format error." << endl;
             return False;
           }
        //    if(ndata>=0)
            {
              float  ysouth=SouthCoordinate[index][iwire].getY();
              float  ynorth=NorthCoordinate[index][iwire].getY();
              SouthCoordinate[index][iwire].setY(ysouth-misalign );
              NorthCoordinate[index][iwire].setY(ynorth-misalign);
          GlobalX[index][iwire] = calculateGlobalX(index, iwire);
          GlobalY[index][iwire] = calculateGlobalY(index, iwire);
            }
    }

  return True;
}

