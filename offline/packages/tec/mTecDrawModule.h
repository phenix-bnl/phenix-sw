#ifndef __MTECDRAWMODULE_H__
#define __MTECDRAWMODULE_H__
#include "phool.h"

#include "GetUniqueName.h"

class PHCompositeNode;
class TecCalibrationObject;
class TecGeometryObject;
class TDragZoomTH2F;

//The following enumerated types are to be used with
//mTecDrawModule's Draw methods to specify which
//side(s) and sectors to draw.

enum EPHENIXSide {kSouth = 0, kNorth = 1, kBoth = 2};
enum EPHENIXSector {kSector0 = 0, kSector1 = 1, kSector2 = 2, kSector3 = 3};

/** 
    Draws TEC Hits and Tracks.
    Detailed documentation: Not available yet.
*/
class mTecDrawModule
{

public:
///
  mTecDrawModule();
///
  virtual ~mTecDrawModule(){}
///
  PHBoolean event(PHCompositeNode*);
///
  PHBoolean Draw(PHCompositeNode*, EPHENIXSector, EPHENIXSide);
///
  PHBoolean Draw(PHCompositeNode*, EPHENIXSector, EPHENIXSide, TecGeometryObject*, TecCalibrationObject*);
///
  PHBoolean Draw(PHCompositeNode*, EPHENIXSector, EPHENIXSide, int delay);
///
  PHBoolean Draw(PHCompositeNode*, EPHENIXSector, EPHENIXSide, int, const char*);
///
  PHBoolean Draw(PHCompositeNode*, EPHENIXSector, EPHENIXSide, int, const char*, int delay);
///
  PHBoolean Draw(PHCompositeNode*, EPHENIXSector, EPHENIXSide, int, const char*, int, TecGeometryObject*, TecCalibrationObject*);
///

/** Draw the specified sector and side of the specified event
    into the current pad.
    Return a pointer to the TDragZoomTH2F used to draw the axes
    of the event display. This allows callers access to the
    TDragZoomTH2F's ResetView() method.
    All objects created in the drawing of the event, including the
    returned TDragZoomTH2F, will have had their kCanDelete bits set.
*/
 TDragZoomTH2F* DrawEventInCurrentPad(PHCompositeNode * root,
                                      int sector,
                                      int side);

 TDragZoomTH2F* DrawEventInCurrentPad(PHCompositeNode * root,
                                      int sector,
                                      int side, TecGeometryObject*, TecCalibrationObject*);

///
  void set_Verbose(int verbose){Verbose=verbose;}
///
  void set_BinThreshold(int th){BinThreshold=th;}
///
  void set_DrawXY(int isect, float* xy) {
   if(isect>=0 && isect<5) {
    DrawXY[isect][0]=xy[0]; DrawXY[isect][1]=xy[1];
    DrawXY[isect][2]=xy[2]; DrawXY[isect][3]=xy[3];
   }
  }
///
  void set_DrawSize(int isect, int* size) {
   if(isect>=0 && isect<5) {
    DrawSize[isect][0]=size[0]; DrawSize[isect][1]=size[1];
   }
  }
///
  void set_DrawMarkerSize(float s) {DrawMarkerSize=s;}


private:

///
  float BinThreshold;

///
  int Verbose;

/// Drawing parameters (sector number)(x1,y1,x2,y2)
  float DrawXY[5][4]; 

/// Display size
  int DrawSize[5][2];

///
  float DrawMarkerSize;

///fNumberOfCalls keeps track of how many times the main Draw method has been called.
  int fNumberOfCalls; //Used in getting unique names for objects.

  /**
   * Draw a display of the specified event in the specified sector
   * and side of the TEC in a new TCanvas. All of the objects created 
   * to make the display have their kCanDelete bit set.
   */
  void DrawEventInNewWindow(PHCompositeNode * root,
                 EPHENIXSector Sector,
                 EPHENIXSide Side,
                 int eventNumber,
                 const char *fname,
                 int x,
                 int y);

  void DrawEventInNewWindow(PHCompositeNode * root,
                 EPHENIXSector Sector,
                 EPHENIXSide Side,
                 int eventNumber,
                 const char *fname,
                 int x,
                 int y, TecGeometryObject*, TecCalibrationObject*);

  /**
   * Return the name for the given side and sector of
   * the TEC.
   */
  static const char* GetTecNameStringFor(int sector,
                                         int side);

};

#endif /*__MTECDRAWMODULE_H__*/

