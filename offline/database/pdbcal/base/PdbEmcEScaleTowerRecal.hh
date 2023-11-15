#ifndef __PDBEMCESCALETOWERRECAL_HH__
#define __PDBEMCESCALETOWERRECAL_HH__

#include "PdbCalChan.hh"

class PdbEmcEScaleTowerRecal : public PdbCalChan 
{
public:
  PdbEmcEScaleTowerRecal();
  PdbEmcEScaleTowerRecal( const PdbEmcEScaleTowerRecal &c);
  virtual ~PdbEmcEScaleTowerRecal(){};

  PdbEmcEScaleTowerRecal&  operator = (const PdbEmcEScaleTowerRecal &c);

  virtual int  setTwrEScaleStatus(const int arm, 
		       const int sector,
		       const int iy,
		       const int iz, 
		       const int value);

  virtual int  setTwrEScaleFactor(const int arm, 
		       const int sector,
		       const int iy,
		       const int iz, 
		       const float value[4]);

  virtual int getTwrEScaleStatus(const int arm, 
		       const int sector,
		       const int iy,
		       const int iz) const; 
		       

  virtual int getTwrEScaleFactor(const int arm, 
		       const int sector,
		       const int iy,
		       const int iz,
		       float *value) const; 
		       

  virtual void print() const;

private:
  int   _twr_escale_stat[2][4][96][48];        // Energy Scale Factor status
//  float _twr_escale[2][4][96][48];              // Energy Scale Factor
  float _twr_escale[2][4][96][48][4];          // Energy Scale Factor
                                               //  (including non-linear effect)

  ClassDef(PdbEmcEScaleTowerRecal,1);
};

#endif /* __PDBEMCESCALETOWERRECAL_HH__ */
