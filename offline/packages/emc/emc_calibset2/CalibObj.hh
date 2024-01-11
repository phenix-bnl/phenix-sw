#ifndef CalibObj_HH
#define CalibObj_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
#include "TNamedDir.hh"
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>

class CalibObj : public TNamedDir {
protected:
  int _nch;
public:
  CalibObj();
  CalibObj(const char* name, const char* title);
  CalibObj(const char* name, const char* title,int nch);
	   
  ~CalibObj();

  // Basic method
  virtual int Fill(int ch,float x,float y = 1.,float z = 1.);
  virtual void Reset(char* option="");
  virtual int Add(CalibObj* th1,Double_t c1 = 1 , char* opt = ""){return 0;};
  virtual void Scale(Double_t c1 = 1){/*    */};
  // Accessor to the data

  // Accessor to the data
  inline int GetNch(){return _nch;};

  ClassDef(CalibObj,1) //Base class for Calibration. TH0Xch,TH1Xch,TH2Xch are inherited from this.
};
//
#endif
//
