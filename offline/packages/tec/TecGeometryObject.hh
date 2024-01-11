#ifndef TECGEOMETRYOBJECT_H
#define TECGEOMETRYOBJECT_H
 
// Created by: Sasha Lebedev (ISU) lebedev@iastate.edu 06/10/99
// Description: Header for TecGeometryObject class

#include <iostream>
#include "phool.h"
#include "PHPoint.h"
#include "PHPanel.h"
#include "PHFrame.h"

#include "TObject.h"
#include "TecBasicObject.hh"

#include "TecAddressObject.hh"

#include "PHGeometry.h"

/**
This is a TEC Geometry Object class. It is used to get coordinates
of a TEC Channel (Wire) using TecAddresObject class. <br>
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/tec/software/geom/index.html}
@author Sasha Lebedev (ISU) 
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
@memo TEC Geometry Class
*/
class TecGeometryObject : public TecBasicObject, public TObject { 

 public:

/// Constructor
  TecGeometryObject(); 

/// Destructor
  ~TecGeometryObject(); 

// member functions

///
  void UseSimulationDatabase();
///
  void UseRealDatabase();

/// Fetch geometry information for this object from a Database
  PHBoolean Fetch();

/// Fetch geometry information for this object from default ASCII file
  PHBoolean FetchFromFile();

/// Fetch geometry information for this object from an ASCII file "phnx.par"
  PHBoolean FetchFromPHNXPAR(const char* filename);

/// Fetch geometry information for this object from a set of ASCII files
  PHBoolean FetchFromFile(const char* filename);

/// Fetch Tec Planes geoemtry from an ASCII file
  PHBoolean FetchPanelsFromFile(const char* filename);

/// Fetch a list of ActivePlanes from an ASCII files
  PHBoolean FetchActivePlanesFromFile(const char* fname);

/// Fetch Sector Frames from an ASCII files
  PHBoolean FetchSectorFramesFromFile(const char* fname);

/// Fetch a list of ActivePlanes from the Database
  PHBoolean FetchActivePlanes();

/// Fetch Sector Frames from the Database
  PHBoolean FetchSectorFrames();

/// Update database from current TGO in memory
  PHBoolean Update(PHTimeStamp* Tbeg, PHTimeStamp* Tend);

/// Update plane coordinates only in the database from current TGO in memory
  PHBoolean UpdatePanels(PHTimeStamp* Tbeg, PHTimeStamp* Tend);

/// Update ActivePlanes only in the database from current TGO in memory
  PHBoolean UpdateActivePlanes(PHTimeStamp* Tbeg, PHTimeStamp* Tend);

/// Update Sector Frames in the database from current TGO in memory
  PHBoolean UpdateSectorFrames(PHTimeStamp* Tbeg, PHTimeStamp* Tend);

/// Read wire misalignments from a file
  PHBoolean ReadMisalignmentFromFile(const char *misfile);

/// Correct wire positions in the database using default misalignment file
  PHBoolean UpdateMisalignment(PHTimeStamp* Tbeg, PHTimeStamp* Tend);

/// Get object name
  const char* getName() {return "TEC Geometry Object";}

///
  float calculateGlobalX(int index, int iwire);
///
  float calculateGlobalY(int index, int iwire);

///
  float getGlobalX(int index, int iwire) {return GlobalX[index][iwire];}
///
  float getGlobalY(int index, int iwire) {return GlobalY[index][iwire];}

/// Get south wire coordinate
  PHPoint getSouthCoordinate(TecAddressObject* TAO);

/// Get south wire coordinate
  PHPoint getSouthCoordinate(int index, int wire);

/// Get north wire coordinate
  PHPoint getNorthCoordinate(TecAddressObject* TAO);

/// Get north wire coordinate
  PHPoint getNorthCoordinate(int index, int wire);

/// Get TEC plane
  PHPanel getTecPlane(int index) { return TecPlane[index]; }

/// Get TEC plane
  PHPanel getTecPlane(TecAddressObject* TAO) { 
    int iplane = TAO->getPlane();
    int isector = TAO->getSector();
    int iside = TAO->getSide();
    int index = iside+iplane*TECMAXSIDE+isector*TECMAXSIDE*TECMAXPLANE;
    return TecPlane[index]; 
  }

/// Get TEC plane
  PHPanel getTecPlane(int iarm, int isector, int iside, int iplane) {
    int index=iside+iplane*TECMAXSIDE+isector*TECMAXSIDE*TECMAXPLANE;
    return TecPlane[index]; 
  }

/// Get TEC plane taking into account rotations and shifts
  PHPanel getTecPanel(int index);

/// Shift the whole east arm
  PHBoolean Shift(PHVector*);

/// Shift the whole east arm
  PHBoolean Shift(float, float, float);

// Get global TEC geometry
  static float get_Zwidth(int iplane) {
           if(iplane==0) return TECZWIDTH1;
           if(iplane==1) return TECZWIDTH2;
           if(iplane==2) return TECZWIDTH3;
           if(iplane==3) return TECZWIDTH4;
           if(iplane==4) return TECZWIDTH5;
           if(iplane==5) return TECZWIDTH6;
           std::cerr << PHWHERE 
		     << "ERROR: Wrong Plane Number." << " " 
		     << iplane << std::endl;
           return -1;
  }

///
  static int get_NumWires(int iplane) {
           if(iplane==0) return TECNUMWIRES1;
           if(iplane==1) return TECNUMWIRES2;
           if(iplane==2) return TECNUMWIRES3;
           if(iplane==3) return TECNUMWIRES4;
           if(iplane==4) return TECNUMWIRES5;
           if(iplane==5) return TECNUMWIRES6;
           std::cerr << PHWHERE
		     << "ERROR: Wrong Plane Number." << " " 
		     << iplane << std::endl;
           return -1;
  }
///
  static float get_WireSpacing(int iplane) {
           if(iplane==0) return TECWIRESPACING1;
           if(iplane==1) return TECWIRESPACING2;
           if(iplane==2) return TECWIRESPACING3;
           if(iplane==3) return TECWIRESPACING4;
           if(iplane==4) return TECWIRESPACING5;
           if(iplane==5) return TECWIRESPACING6;
           std::cerr << PHWHERE
		     << "ERROR: Wrong Plane Number." << " " 
		     << iplane << std::endl;
           return -1;
  }
///
  static float get_WireRadius(int iplane) {
//   TECRADIUS(iplane)       -> middle of the TEC plane in GEANT
//   TECMIDPOS=8.9166        -> distance from the middle of the TEC plane
//                              to inner TEC plane edge
//   TECYWIDTH=3.7032        -> TEC plane width in GEANT
//   TECTRDWIDTH=7.065       -> TRD plane width in GEANT
//   TECWIREDIST=0.3         -> distance from outer plane edge to the wire
  	   if(iplane==0) return TECRADIUS1-TECMIDPOS+TECYWIDTH+TECTRDWIDTH-TECWIREDIST;
           if(iplane==1) return TECRADIUS2-TECMIDPOS+TECYWIDTH+TECTRDWIDTH-TECWIREDIST;
           if(iplane==2) return TECRADIUS3-TECMIDPOS+TECYWIDTH+TECTRDWIDTH-TECWIREDIST;
           if(iplane==3) return TECRADIUS4-TECMIDPOS+TECYWIDTH+TECTRDWIDTH-TECWIREDIST;
           if(iplane==4) return TECRADIUS5-TECMIDPOS+TECYWIDTH+TECTRDWIDTH-TECWIREDIST;
           if(iplane==5) return TECRADIUS6-TECMIDPOS+TECYWIDTH+TECTRDWIDTH-TECWIREDIST;
           std::cerr << PHWHERE
		     << "ERROR: Wrong Plane Number." << " " 
		     << iplane << std::endl;
           return -1;
  }
///
  static float get_Radius(int iplane) {
  	   if(iplane==0) return TECRADIUS1;
           if(iplane==1) return TECRADIUS2;
           if(iplane==2) return TECRADIUS3;
           if(iplane==3) return TECRADIUS4;
           if(iplane==4) return TECRADIUS5;
           if(iplane==5) return TECRADIUS6;
           std::cerr << PHWHERE
		     << "ERROR: Wrong Plane Number." << " " 
		     << iplane << std::endl;
           return -1;
  }
///
  static int get_ActivePlane(int iplane) {
           if(iplane==0) return 1;
           if(iplane==1) return 1;
           if(iplane==2) return 1;
           if(iplane==3) return 1;
           if(iplane==4) return 0;
           if(iplane==5) return 0;
           std::cerr << PHWHERE
		     << "ERROR: Wrong Plane Number." << " " 
		     << iplane << std::endl;
           return -1;
  }
///
  static float get_ywidth() { return TECYWIDTH; }
///
  static int get_sectperarm() { return TECMAXSECT; }
///
  static float get_phibote() { return TECPHIBOTE; }
///
  static float get_phitope() { return TECPHITOPE; }
///
  static float get_phibotw() { return TECPHIBOTW; }
///
  static float get_phitopw() { return TECPHITOPW; }

///
  PHBoolean setEastCarriage(PHFrame &f);
///
  PHBoolean setTecPlaneFrame(int index, PHFrame &f);
///
  PHBoolean setTecSectorFrame(int index, PHFrame &f);
///
  PHFrame getEastCarriage() {return EastCarriage;}
///
  PHFrame getTecPlaneFrame(int index) {return TecPlaneFrame[index];} 
///
  PHFrame getTecSectorFrame(int index) {return TecSectorFrame[index];} 

///
  PHBoolean isActivePlane(int index) { 
      if(index>-1 && index<TECMAXINDEX) {return ActivePlane[index];}
        else {return False;}
  }
///
  PHBoolean isActiveSectorSide(int index) {
      if(index>-1 && index<TECMAXSECT*TECMAXSIDE) {return ActiveSectorSide[index];}
        else {return False;}
  }
///
  void setActiveSectorSide(int index, int active) {
       if(index>-1 && index<TECMAXSECT*TECMAXSIDE) { 
                        ActiveSectorSide[index]=active; 
       }
  }
///
  void setActivePlane(int index, int active) {
       if(index>-1 && index<TECMAXINDEX) { 
                        ActivePlane[index]=active; 
       }
  }

private:
/// 
 PHPoint SouthCoordinate[TECMAXINDEX][TECMAXWIRE];
///
 PHPoint NorthCoordinate[TECMAXINDEX][TECMAXWIRE];
///
 float GlobalX[TECMAXINDEX][TECMAXWIRE];
///
 float GlobalY[TECMAXINDEX][TECMAXWIRE];
///
 PHPanel TecPlane[TECMAXINDEX];
///
 PHPanel TecPlane_[TECMAXINDEX];
///
 int ActivePlane[TECMAXINDEX];
///
 int ActiveSectorSide[TECMAXSECT*TECMAXSIDE];
///
 PHFrame EastCarriage;
///
 PHFrame TecPlaneFrame[TECMAXINDEX];
///
 bool tecplane_calculated[TECMAXINDEX];
///
 PHFrame TecSectorFrame[TECMAXSECT];

}; 

#endif /* TECGEOMETRYOBJECT_H */ 


