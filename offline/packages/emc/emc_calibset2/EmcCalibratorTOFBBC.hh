#ifndef EmcCalibratorTOFBBC_HH
#define EmcCalibratorTOFBBC_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
#include "EmcCalibrator.hh"
#include "EmcCalibratorTOF.hh"
#include "EmcCalibratorTOFObj.hh"

class EmcCalibratorTOFBBC : public EmcCalibratorTOFObj {
public:
  TH1F _h_twr_t0;
  TH1F _h_twr_s0;
  TH1F _h_twr_fit[3];
public:
  EmcCalibratorTOFBBC(): EmcCalibratorTOFObj(){/* */};
  EmcCalibratorTOFBBC(const char* name, const char* title): EmcCalibratorTOFObj(name,title){/* */};
  EmcCalibratorTOFBBC(const char* name, const char* title,int nch,
		      int xnbins,float xlow,float xup,int ynbins,float ylow,float yup);
  ~EmcCalibratorTOFBBC(){/*   */};


  // Analysis method.
  virtual int AnalyzeTwr(char* opt="");
  virtual int Reset(char* option="");
  bool WriteFileTwr(char* twr_file);

  // Data Accessor
  ClassDef(EmcCalibratorTOFBBC,1)
};
//
#endif
//
