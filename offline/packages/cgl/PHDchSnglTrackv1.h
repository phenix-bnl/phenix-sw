#ifndef __PHDCHSNGLTRACKV1_HH__
#define __PHDCHSNGLTRACKV1_HH__
#include "PHObject.h"

class PHDchSnglTrackv1:public TObject
{
 public:
  PHDchSnglTrackv1();
  virtual ~PHDchSnglTrackv1(){}

  short get_ErrorCode() const {return ErrorCode;}
  void set_ErrorCode(const short ival) {ErrorCode = ival; return;}

  short get_numberOfSuccessfulIterations() const {return numberOfSuccessfulIterations;}
  void set_numberOfSuccessfulIterations(const short ival) {numberOfSuccessfulIterations = ival; return;}

  short get_numberOfX1X2hitsFitted() const {return numberOfX1X2hitsFitted;}
  void set_numberOfX1X2hitsFitted(const short ival) {numberOfX1X2hitsFitted = ival; return;}


  float get_chi2() const {return chi2;}
  void set_chi2(const float rval) {chi2 = rval; return;}

  float get_fittedAlpha() const {return fittedAlpha;}
  void set_fittedAlpha(const float rval) {fittedAlpha = rval; return;}

  float get_fittedBeta() const {return fittedBeta;}
  void set_fittedBeta(const float rval) {fittedBeta = rval; return;}

  float get_fittedPhi() const {return fittedPhi;}
  void set_fittedPhi(const float rval) {fittedPhi = rval; return;}

  float get_fittedPhi0() const {return fittedPhi0;}
  void set_fittedPhi0(const float rval) {fittedPhi0 = rval; return;}

  float get_fittedTheta0() const {return fittedTheta0;}
  void set_fittedTheta0(const float rval) {fittedTheta0 = rval; return;}

  float get_momentum() const {return momentum;}
  void set_momentum(const float rval) {momentum = rval; return;}

 protected:

  short ErrorCode;
  short numberOfSuccessfulIterations;
  short numberOfX1X2hitsFitted;

  float chi2;
  float fittedAlpha;
  float fittedBeta;
  float fittedPhi;
  float fittedPhi0;
  float fittedTheta0;
  float momentum;

  ClassDef(PHDchSnglTrackv1,1) 

};

#endif /* __PHDCHSNGLTRACKV1_HH__ */
