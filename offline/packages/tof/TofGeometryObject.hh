//-----------------------------------------------------------------------------
//  Declaration of class TofGeometryObject
//
//  Purpose: Geometry DB organization for Time-of-Flight
//
//  Description: TOF Geometry Object
//
//  Author: Akio Kiyomichi (Univ.of Tsukuba)
//
//  History: 04/12/00  A.Kiyomichi  First Version
//           07/04/00  A.Kiyomichi  add setEastCarriage
//           09/16/00  A.Kiyomichi  add coordinate system (thanks Julia)
//           10/29/01  T.Chujo      Using namespace PHGeometry 
//           10/29/01  A.Kiyomichi  add TofAddress [remove from **.cc]
//-----------------------------------------------------------------------------

#ifndef __TOFGEOMETRYOBJECT_HH__
#define __TOFGEOMETRYOBJECT_HH__

#include "Tof.hh"
#include "TofBasicObject.hh"
#include "PHGeometry.h"

class PHCompositeNode;
class TofAddressObject;

const float TOF_RDIST_SLATPANEL = 3.2905;  // distance between slat and panel
const float TOF_SCINTZ_0 = 69.631;         // rslat = 3.2905
const float TOF_SCINTZ_1 = 11.606;         // rslat = 3.2905
const float TOF_SCINTZ_2 = 56.609;         // rslat = 3.2905

/** 
This is TOF Geometry Object class.  <br> 
Created: 04/12/00. 
@author Akio Kiyomichi (Univ. of Tsukuba)
<a href="mailto:kiyo@bnl.gov">kiyo@bnl.gov</a> 
@memo TOF Geometry Object Class 
*/

class TofGeometryObject : public TofBasicObject
{
public:
  TofGeometryObject();
  virtual ~TofGeometryObject();

  // Get Name
  PHString getName() {return "TOF Geometry Object";} 

  // Set
  PHBoolean setEastCarriage(float x, float y, float z);
  PHBoolean setEastCarriage(PHPoint p);
  PHBoolean setEastCarriage(PHFrame &f);
  PHBoolean setPanelFrame(int ipanel_seq, PHFrame &f);

  PHBoolean setPanelGeo(int ipanel_seq, PHPanel p); // Set Panel Geometry
  PHBoolean setPanelGeo(int ipanel_seq, PHPoint p0, PHPoint p1, PHPoint p2);
  PHBoolean setPanelErr(int ipanel_seq, PHPoint p0, PHPoint p1, PHPoint p2);

  PHBoolean setPointOffset(int slatid,PHPoint p){ 
    pointOffset[slatid] = p; 
    return True;
  }
  PHBoolean setVectorOffset(int slatid,PHVector v){
    vectorOffset[slatid] = v;
    return True;
  }

  // East Carriage
  //PHPoint     getEastCarriage(){ return EastCarriage;}
  float       getEastCarriageX(){ 
    return (float) EastCarriage.getOrigin().getX();}
  //float       getEastCarriageY(){ return (float) EastCarriage.getY();}
  //float       getEastCarriageZ(){ return (float) EastCarriage.getZ();}
  PHFrame     getEastCarriage(){ return EastCarriage;}
  PHFrame     getPanelFrame(int ipanel_seq){ 
    return TofPanelFrame[ipanel_seq];}

  // TOF Panel & Slat
  PHPanel     getPanelGeo(int panel_seq);
  PHLine      getSlatGeo(int slatid);
  PHPoint     getSlatXYZ(int slatid); // [x,y,z] center point
  PHCylPoint  getSlatRPhiZ(int slatid); // [r,phi,z] center point
  PHVector    getSlatVector(int slatid);  // [dx,dy,dz] 
  float getSlatLength(int slatid);  // scinti. long = 63.77cm, short = 43.39cm
  float getSlatWidth(int slatid);   // scinti. width 1.5cm X 1.5cm

  float getXpos(int slatid);        // 
  float getYpos(int slatid);        // 
  float getZpos(int slatid);        // 
  float getRadius(int slatid);      // 
  float getPhi(int slatid);         // 

  void setGeomPanelName(PHString string) { setGeom0Name(string); } 
  void setGeomSlatOffName(PHString string) { setGeom1Name(string); } 

  // Fetch information from Objy Database 
  PHBoolean fetch();   // fetchPanel() + fetchSlatOff()
  PHBoolean fetch(const int run);
  
  PHBoolean fetchEastCarriage();
  PHBoolean fetchPanel();
  PHBoolean fetchSlatOff();
  // Update information to Objy Database 
  PHBoolean update(const int beginrun, const int endrun);
  PHBoolean update(PHTimeStamp tStart, PHTimeStamp tStop);
  PHBoolean updateEastCarriage(PHTimeStamp tStart, PHTimeStamp tStop);
  PHBoolean updatePanel(PHTimeStamp tStart, PHTimeStamp tStop);
  PHBoolean updateSlatOff(PHTimeStamp tStart, PHTimeStamp tStop);

  // Fetch Geometry information from Table
  PHBoolean fetchGeoTable(PHCompositeNode *top);
  // Write FEM map information to Table
  PHBoolean writeGeoTable(PHCompositeNode *top);

  // Fetch information from default ASCII file
  PHBoolean fetchFromFile();
  // Fetch information from an ASCII file "filename" 
  PHBoolean fetchFromFile(const char* filename);
  PHBoolean fetchFromFile(const char* filename, const char* offsetfile);
  // Write information to an ASCII file "filename" 
  PHBoolean writeToFile(const char* filename);
  PHBoolean writeToFile(const char* filename, const char* offsetfile);

  void print();
  void print(int id);

private:
  TofAddressObject*     TofAddress;

  PHBoolean fromPanelToSlat(int ipanel_seq, PHPanel panel);

  // Coordinate System 
  PHFrame   EastCarriage;                  // east carriage coordinate system
  PHFrame   TofPanelFrame[TOF_NPANEL_ALL]; // coordinate system for each panel
  // Geometry
  PHPanel   TofPanel[TOF_NPANEL_ALL];      // Panel Geometry
  PHLine    TofSlat[TOF_NSLAT_ALL];        // Slat Geometry
  PHPoint   TofSlatCenter[TOF_NSLAT_ALL];  // Slat center point
  PHVector  TofSlatVector[TOF_NSLAT_ALL];  // Slat vector

  PHPoint   panelError[3][TOF_NPANEL_ALL]; // error value for each panel
  PHPoint   pointOffset[TOF_NSLAT_ALL];    // position offset for each slat
  PHPoint   vectorOffset[TOF_NSLAT_ALL];   // direction offset for each slat

};

#endif /* __TOFGEOMETRYOBJECT_HH__ */
