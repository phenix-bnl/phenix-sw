#ifndef __PDBEMCT0SECTOR_HH__
#define __PDBEMCT0SECTOR_HH__

#include "PdbCalChan.hh"

class PdbEmcT0Sector : public PdbCalChan 
{
public:
  PdbEmcT0Sector();

  PdbEmcT0Sector& operator = (const PdbEmcT0Sector &c);

  virtual ~PdbEmcT0Sector(){};

  virtual int  setSectorT0Status(const int arm, 
				 const int sector,
				 const int value);

  virtual int  setSectorT0Correction(const int arm, 
				     const int sector,
				     const float value);

  virtual int  setSectorT0Error(const int arm, 
				const int sector,
				const float value);

  virtual int getSectorT0Status(const int arm, 
				const int sector) const; 
		       
  virtual float getSectorT0Correction(const int arm, 
				      const int sector) const; 
		       
  virtual float getSectorT0Error(const int arm, 
				 const int sector) const; 
  virtual void print() const;

private:
  int   _run_t0_stat[2][4];       // status
  float _run_t0[2][4];            // T0 correction
  float _run_t0_error[2][4];      // error of the correction

  ClassDef(PdbEmcT0Sector,1);
};

#endif /* __PDBEMCT0SECTOR_HH__ */
