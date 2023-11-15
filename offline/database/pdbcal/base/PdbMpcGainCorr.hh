//-----------------------------------------------------------------------------
//
//  Database definition of Gain Correction
//
//  $Id: PdbMpcGainCorr.hh,v 1.1 2009/02/12 01:14:26 chiu Exp $
//-----------------------------------------------------------------------------
#ifndef __PDBMPCGAINCORR_HH__
#define __PDBMPCGAINCORR_HH__

#include <PdbCalChan.hh>
#include <iostream>
#include <ctime>
#include <phool.h>

class PdbMpcGainCorr : public PdbCalChan {
public:
  PdbMpcGainCorr();
  virtual ~PdbMpcGainCorr() {}

  PdbMpcGainCorr& operator = (const PdbMpcGainCorr &p);

  void setVersion(Int_t v) { version = v; }

  void setPar(UInt_t i, Float_t p) {
    if ( i>10 )
      {
        std::cerr << PHWHERE << " ERROR, index " << i << " out of bounds" << std::endl;
        return;
      }
    par[i] = p;
  }

  void setChi2(Float_t c) { chi2 =c; }

  Int_t getVersion() const { return version; }

  float getPar(UInt_t i) const {
    if ( i>10 )
      {
        std::cerr << PHWHERE << " ERROR, index " << i << " out of bounds" << std::endl;
        return 0.;
      }
    return par[i];
  }

  float getChi2() const {return chi2;}

  float getGainCorr(const time_t meantime) const;

  virtual void reset();

  virtual void print() const;

private:

  int   version;	// version of fit
  float par[11];
  float chi2;

  ClassDef(PdbMpcGainCorr,1);

};

#endif // __PDBMPCGAINCORR_HH__ 

