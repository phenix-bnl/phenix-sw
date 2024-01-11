#ifndef EmcCalibratorTOFMIP_HH
#define EmcCalibratorTOFMIP_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
#include "EmcCalibrator.hh"
#include "EmcCalibratorTOF.hh"
#include "EmcCalibratorTOFObj.hh"
#include "fitpeak.hh"
#define FITPEAK_TOFMIP_DOWNSIGMA 2
#define FITPEAK_TOFMIP_UPSIGMA 2
#define PI_PIDCUT_DOWN -5
#define PI_PIDCUT_UP    2

class EmcCalibratorTOFMIP : public EmcCalibratorTOFObj {
public:
  TH1F _h_twr_mip;
  TH1F _h_twr_mipfit[4];
protected:
  float _pi_pidcut[2];
public:
  EmcCalibratorTOFMIP();
  EmcCalibratorTOFMIP(const char* name, const char* title);
  EmcCalibratorTOFMIP(const char* name, const char* title,int nch,
		      int xnbins,float xlow,float xup,int ynbins,float ylow,float yup);
  ~EmcCalibratorTOFMIP(){/*   */};

  // Analysis method.
  virtual int AnalyzeTwr(char* opt="");

  // Data Accessor
  void SetFitpeakSigma(float down,float up){_fit_sigma[0]=down;_fit_sigma[1]=up; };
  void Set_PIPID_TOFCut(float down,float up){_pi_pidcut[0]=down; _pi_pidcut[1]=up; };
  bool WriteFileTwr(char* file);

  ClassDef(EmcCalibratorTOFMIP,1)
};
//
#endif
//
