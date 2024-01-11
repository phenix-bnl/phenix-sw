#ifndef __TOFWGEOMETRY_H__
#define __TOFWGEOMETRY_H__

#include "TofwPar.h"
#include "PHString.h"
#include <string>
#include "phool.h"
#include "PHPoint.h"
#include "PHPanel.h"

#include "PHGeometry.h"

class PHTimeStamp;

class TofwGeometry
{
 public:
  TofwGeometry();
  virtual ~TofwGeometry() {}

  // Get Name
  PHString getName() {return "TOFW Geometry Object";} 

  // TOFW Box, Panel, Slat
  PHPanel     getBoxGeo(int ibox_seq);
  PHPoint     getStripCenter(int ibox, int ichamber, int istrip);
  PHPoint     getSimStripCenter(int ibox, int ichamber, int istrip);

 private:
  PHString geometryName;  // geometry name
  int isGeometryOK;       // geometry flag
  int debug;              // debug flag

  // Geometry
  PHPanel   BoxPanel[4];         // BoxPanel used for TOFW acceptance region
};

#endif
