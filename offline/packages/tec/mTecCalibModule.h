#ifndef __MTECCALIBMODULE_H__
#define __MTECCALIBMODULE_H__

#include "phool.h"

class PHCompositeNode;
class TecAddressObject;
class TecCalibrationObject;
/**
Reads Calibration Constants from a Database and converts
TEC Raw Data to Calibrated Data.
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/tec/calib.html}
*/

class mTecCalibModule
{
public:
///
  mTecCalibModule();
///
  ~mTecCalibModule();
///
  PHBoolean event(PHCompositeNode*);
///
  PHBoolean event(PHCompositeNode*, TecAddressObject*, TecCalibrationObject*);
///
  PHBoolean event(PHCompositeNode*, TecAddressObject*, TecCalibrationObject*, TecCalibrationObject*);
///
  PHBoolean mixdst(PHCompositeNode*);
///
  void set_Verbose(int verbose){Verbose=verbose;}
///
  void set_UseObjy(int useobjy){UseObjy=useobjy;}
///
  void set_DrawScaleX(int tmp){DrawScaleX=tmp;}
///
  void set_DrawScaleY(int tmp){DrawScaleY=tmp;}
///
  void set_DrawCut(int tmp){DrawCut=tmp;}
///
  void set_FillHistos(int tmp){FillHistos=tmp;}
///
  void set_SaveHistosEvent(int tmp){SaveHistosEvent=tmp;}
///
  void set_RunNumber(int tmp){RunNumber=tmp;}
///
  void set_HistCut(int tmp){HistCut=tmp;}
///
  void set_DrawSave2gif(int tmp){DrawSave2gif=tmp;}
///
  void set_InputADC(int tmp){InputADC=tmp;}
///
  void set_ApplyGains(int tmp){ApplyGains=tmp;}

private:
/// 
  int DrawScaleX;
///
  int DrawScaleY;
///
  int DrawCut;
///
  int Verbose;
///
  int UseObjy;
///
  int FillHistos;
///
  int SaveHistosEvent;
///
  int RunNumber;
///
  int HistCut;
///
  int DrawSave2gif;
///
  int InputADC;
///
  int ApplyGains;
};

#endif /*__MTECCALIBMODULE_H__*/

