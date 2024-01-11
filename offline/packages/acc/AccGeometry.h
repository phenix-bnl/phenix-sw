
#ifndef __ACCGEOMETRY_H_
#define __ACCGEOMETRY_H_

#include "Acc.h"

#include "PHFrame.h"
#include "PHLine.h"
#include "PHPanel.h"
#include "PHPoint.h"

#include "PHString.h"
#include "phool.h"

class PHTimeStamp;

class AccGeometry
{

 public:
  AccGeometry();
  virtual ~AccGeometry() {}

  // Geometry name
  PHString get_geomName() const {return geomName;}
  void set_geomName(const char* name) {geomName = name;}
  PHString getDescription() const {return "ACC BOX Geometry";}

  // Set geometry
  PHBoolean setPanelFrame(int ipanel, PHFrame &frame);
  PHBoolean setDetectorFrame(PHFrame& frame);
  PHBoolean setPanelGeo(int ipanel, PHPanel panel); // set panel geometry
  PHBoolean setPanelGeo(int ipanel, PHPoint p0, PHPoint p1, PHPoint p2);
  PHBoolean setPanelErr(int ipanel, PHPanel panel);
  PHBoolean setPanelErr(int ipanel, PHPoint p0, PHPoint p1, PHPoint p2);

  // Get geometry
  PHFrame getPanelFrame(int ipanel) { return AccPanelFrame[ipanel]; }
  PHFrame getDetectorFrame() { return DetectorFrame; }
  PHPanel getPanelGeo(int ipanel) { return AccPanel[ipanel]; }

  // fetch
  PHBoolean fetch(const int run); // fetch geometry from run number
  PHBoolean fetch(const char* filename); // fetch geometry info. from file.

  // write to file
  PHBoolean write(const char* filename="AccCalib.geo.new");   // write geometry to file.

  // update parameter
  PHBoolean update(PHTimeStamp& tStart, PHTimeStamp& tStop);

  PHPoint fromLocalToGlobal(int ipanel, PHPoint& local);

  // get position
  float getX(const int ibox) const;
  float getY(const int ibox) const;
  float getZ(const int ibox) const;

  // Print 
  void print();
  void print(const int id);

  void set_debug(const int ival) {debug = ival;}

 private:

  PHString geomName;  // geometry name
  int isGeoOK; // geometry flag
  int debug;   // debug flag

  // coordinate system
  PHFrame DetectorFrame;           // west carriage coordinate system
  PHFrame AccPanelFrame[ACC::ACC_NBOX]; // coordinate system for each panel

  // geometry
  PHPanel AccPanel[ACC::ACC_NBOX];      // panel geometry
  PHPanel AccPanelErr[ACC::ACC_NBOX];   // panel geometry error
  PHPoint panelErr[3][ACC::ACC_NBOX];
  PHBoolean fromFile;              // flag to indicate the init source

};

#endif
