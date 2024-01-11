
#ifndef __TECUTILITIES_H__
#define __TECUTILITIES_H__

#include "TecBasicObject.hh"

class PHCompositeNode;
class PHLine;
class PHPoint;
class TecClusterContainer;
class TecGeometryObject;
class TecCalibrationObject;
class TecProj;

namespace TecUtilities {

///
  void copyTecOut(PHCompositeNode*, int, int, int);
///
  void copyTecTrackOut(PHCompositeNode*);
///
  void copyTecHitOut(PHCompositeNode*, int);
///
  void copyTecOutV1(PHCompositeNode*, TecGeometryObject*, TecCalibrationObject*);
///
  int CalcXYfromBin(float relativebin, float Xwire, float Ywire,
                    float Sinus, float &Xpos, float &Ypos, int Debug);
/// 
  int GetMidPhi(float MidPhi[TECMAXINDEX], 
	        float midphi[TECMAXSECT*TECMAXSIDE],
	        float SinAlpha[TECMAXINDEX],
                TecGeometryObject* TGO, int Debug);
///
  int GetMinMax(int MinBin[TECMAXINDEX], int MaxBin[TECMAXINDEX],
                TecCalibrationObject* TCO, int Debug);
///
  float Ampl2Charge(int ampl);

///
  int Charge2Ampl(float charge);

PHPoint
  get_projection(PHLine dcline, int ichamber, TecGeometryObject* TGO);

bool
  Associate(int icgl, PHLine dcline,
	    TecClusterContainer* tec, TecCalibrationObject* TCO,
	    TecGeometryObject* TGO, TecProj* tecproj,
	    bool is_swapped = false,
	    float min_distance_cut = 4.0);

}

#endif 

