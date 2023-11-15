#ifndef __PDBEMCESCALETOWER_HH__
#define __PDBEMCESCALETOWER_HH__

#include "PdbCalChan.hh"

class PdbEmcEScaleTower : public PdbCalChan 
{
public:
  PdbEmcEScaleTower();
  PdbEmcEScaleTower( const PdbEmcEScaleTower &c);
  virtual ~PdbEmcEScaleTower(){};

  PdbEmcEScaleTower&  operator = (const PdbEmcEScaleTower &c);

  virtual int  setTwrEScaleStatus(const int arm, 
		       const int sector,
		       const int iy,
		       const int iz, 
		       const int value);

  virtual int  setTwrEScaleFactor(const int arm, 
		       const int sector,
		       const int iy,
		       const int iz, 
		       const float value);

  virtual int getTwrEScaleStatus(const int arm, 
		       const int sector,
		       const int iy,
		       const int iz) const; 
		       

  virtual float getTwrEScaleFactor(const int arm, 
		       const int sector,
		       const int iy,
		       const int iz) const; 
		       

  virtual void print() const;

private:
  int   _twr_escale_stat[2][4][96][48];        // Energy Scale Factor status
  float _twr_escale[2][4][96][48];              // Energy Scale Factor

  ClassDef(PdbEmcEScaleTower,1);
};

#endif /* __PDBEMCESCALETOWER_HH__ */
