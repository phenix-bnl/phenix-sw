//--------------------------------------------------- 
// Class: hbdDetectorGeo
// 
// Created by: Jeffery T. Mitchell
// 
// Description: Central arm global geometry generator
// 
// update: 3/28/00
// update: 4/15/03  simulation geometry in OBJY database, Indrani Ojha
// update: 12/31/03 add method to access fromSimDatabase, not yet in use (CFM)
//--------------------------------------------------- 


#ifndef __HBDDETECTORGEO_HH__
#define __HBDDETECTORGEO_HH__

#include <Hbd.h>
#include <phool.h>
#include <PHPanel.h>
#include <PHTriPanel.h>
#include <PHAngle.h>
#include <PHPoint.h>
class PHCompositeNode;
class PHTimeStamp;


class hbdDetectorGeo{ 

public:
  hbdDetectorGeo();                             // constructor
  virtual ~hbdDetectorGeo();                            // destructor
  
public:

  // Data member access methods
  void set_Verbose(short);             // Set Verbose
  short get_Verbose();                 // Get Verbose
  void set_hbdActive(short, short *);  // Set the HBD active list for an arm
  void get_hbdActive(short, short *);  // Get the HBD active list for an arm

  short debug;
  //database code updated 3/6/2006
  PHBoolean fetch(const int run);
  PHBoolean update(PHTimeStamp& tStart, PHTimeStamp& tStop);

  PHBoolean fetchPad(const int run);
  PHBoolean fetchPad(PHTimeStamp& tNow);
  PHBoolean updatePad(PHTimeStamp& tStart, PHTimeStamp& tStop);

  // The return value are the number of PHPanel objects returned
  short get_hbdGeo(short, PHPanel *);

  PHBoolean setPanelGeo(int sect, const PHPanel& ipanel);
  const PHPanel& getPanelGeo(int ipanel);

  void readGeoFromFile(const char *filename);

  void readPadInfoFromFile(const char *filename);

  void printGeo();

  void printPadInfo();

  int LocToGlob(double LocY, double LocZ,
                          double& GlobX, double& GlobY, double& GlobZ,
                          int ipanel);

  void GlobToLoc(double GlobX, double GlobY, double GlobZ,
                          double& LocY, double& LocZ, int& LocSect);

  void getPadInfo(int ADCch, int &arm, int &sec,int &seqsec,int &padid);
  
  void getPadLocal(int padid, double & LocY, double & LocZ);
  void getPadGlobal(int padid, int sect,double & GlobX,  double & GlobY, double & GlobZ);


  void PadInfo();

  void PanelToSect(int ipanel, short & arm, short & sector);
  
  void FindNearestPanel(double X, double Y, double Z, int & ipanel);

 
  void FindNearestPad(double LocY, double LocZ, int & padid);
  void FindNearestPad(double GlobX, double GlobY, double GlobZ, int & padid);

  double DistanceFromPanel(double X,double Y, double Z, int ipanel);

private:

  // The geometry objects representing each sector
  PHPanel hbdSectors[2][HBD::HBD_NSECTARM];

  // Verbosity level for this class
  short Verbose;

  // The following pertain to default geometry generation
  // Flag of active sectors
  short hbdActive[2][HBD::HBD_NSECTARM];

  double pad_center[192][2];
  short pad_type[192];

  struct PadGeom{
    int ADCch;
    int arm;
    int sec;
    int seqsec;
    int padid;
    double CenterInLocalY;
    double CenterInLocalZ;
    double globalX;
    double globalY;
    double globalZ;
  } padgeom[2304];
};

#endif /* __HBDDETECTORGEO_HH__ */










