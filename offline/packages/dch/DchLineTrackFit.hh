#ifndef __DCHLINETRACKFIT_H__
#define __DCHLINETRACKFIT_H__

//////////////////////////////////////////////////////////////////////////////
//
// Class:  DchLineTrackFit class which
//         inherits from TObject and utilizes TMinuit
//
// By:  Julia Velkovska and Jane Burward-Hoy
//
// Description:  Uses matrix class from PHGeometry and "mini" from TMinuit 
//               to determine the best fit basepoint and direction vector 
//               of the track given initial guesses of these six parameters.
//
// Date:  07/10/00
//
//////////////////////////////////////////////////////////////////////////////

#include "phool.h"
#include "PHDchGeometryObject.h"
#include "Rtypes.h"
#include "TMinuit.h"

class PHPoint;
class PHVector;

const int NUMPAR = 6;
const double RESO = 0.0150;  // hit resolution in cm

void minimizeME(Int_t &npar, 
		Double_t *grad, Double_t &sum, 
		Double_t *par, Int_t iflag); 

class DchLineTrackFit: public TObject {

public:

  DchLineTrackFit(const PHLine &guess,PHPointerList<PHLine> *hits);
  virtual ~DchLineTrackFit();

  PHBoolean fitTrack();
  PHLine getTrack() { return track;}
  double getRedChi2() { return redchi2; }
  double getIter() { return iter; }
  double getEDM() { return edm;}
  double getSigma() {return sigma;}  // sigma of hit for chi2 calculation 
  void   setParStep(int &i, Double_t &val) { parStep[i] = val; }  
  Double_t getParStep(int &i) const { return parStep[i]; }
  Double_t getPar(int &i) const { return par[i]; }
  int getNumPar() { return NUMPAR; }
  void print() const;

  // called by minimizeMe
  const TMinuit* getMinuit() const { return gMinuit;}
  int getNumOfHits() const { return hitLines->length();}
  PHPoint getHitBasepoint(int &i) const { return (*(*hitLines)[i]).getBasepoint(); }
  PHVector getHitDirection(int &i) const { return (*(*hitLines)[i]).getDirection(); }

protected:

  void setMinuit(TMinuit* min) { gMinuit = min;}
  int getFitStatus (char *Migrad);

private:

  TMinuit *gMinuit; 
  PHDchGeometryObject *geometry;
  PHPoint basepoint;
  PHVector direction;
  PHPointerList<PHLine> *hitLines;
  Double_t par[6];
  Double_t parStep[6];
  PHLine track,previous;
  Double_t redchi2; // reduced chi2
  Int_t iter;
  Double_t edm;
  PHBoolean fitOkay;
  double sigma;

};
#endif /*__DCHLINETRACKFIT_H__*/
