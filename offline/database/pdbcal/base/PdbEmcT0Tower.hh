#ifndef __PDBEMCT0TOWER_HH__
#define __PDBEMCT0TOWER_HH__

#include "PdbCalChan.hh"

class PdbEmcT0Tower : public PdbCalChan 
{
public:
  PdbEmcT0Tower();
  virtual ~PdbEmcT0Tower(){};

  PdbEmcT0Tower&  operator = (const PdbEmcT0Tower &c);

  virtual int  setTwrT0Status(const int arm, 
			      const int sector,
			      const int iy,
			      const int iz, 
			      const int value);

  virtual int  setTwrT0Correction(const int arm, 
				  const int sector,
				  const int iy,
				  const int iz, 
				  const float value);

  virtual int  setTwrT0Error(const int arm, 
			     const int sector,
			     const int iy,
			     const int iz, 
			     const float value);

  virtual int  setLeastCountStatus(const int arm, 
				   const int sector,
				   const int iy,
				   const int iz, 
				   const int value);

  virtual int  setLeastCountCorrection(const int arm, 
				       const int sector,
				       const int iy,
				       const int iz, 
				       const float value);

  virtual int  setLeastCountError(const int arm, 
				  const int sector,
				  const int iy,
				  const int iz, 
				  const float value);

  virtual int  setSlewingStatus(const int arm, 
				const int sector,
				const int iy,
				const int iz, 
				const int value);

  virtual int  setSlewingCorrection(const int arm, 
				    const int sector,
				    const int iy,
				    const int iz, 
				    const float value);

  virtual int  setSlewingError(const int arm, 
			       const int sector,
			       const int iy,
			       const int iz, 
			       const float value);

  virtual int getTwrT0Status(const int arm, 
			     const int sector,
			     const int iy,
			     const int iz) const; 
		       
  virtual float getTwrT0Correction(const int arm, 
				   const int sector,
				   const int iy,
				   const int iz) const; 
  
  virtual float getTwrT0Error(const int arm, 
			      const int sector,
			      const int iy,
			      const int iz) const; 
  
  virtual int getLeastCountStatus(const int arm, 
				  const int sector,
				  const int iy,
				  const int iz) const; 
		       
  virtual float getLeastCountCorrection(const int arm, 
					const int sector,
					const int iy,
					const int iz) const; 
		       
  virtual float getLeastCountError(const int arm, 
				   const int sector,
				   const int iy,
				   const int iz) const; 

  virtual int getSlewingStatus(const int arm, 
			       const int sector,
			       const int iy,
			       const int iz) const; 

  virtual float getSlewingCorrection(const int arm, 
				     const int sector,
				     const int iy,
				     const int iz) const; 

  virtual float getSlewingError(const int arm, 
				const int sector,
				const int iy,
				const int iz) const; 

  virtual void print() const;

private:
  int   _twr_t0_stat[2][4][96][48];        // T0 correction status
  float _twr_t0[2][4][96][48];              // T0 correction
  float _twr_t0_error[2][4][96][48];              // T0 correction

  int   _twr_lc_stat[2][4][96][48];         // Least count correction status
  float _twr_lc[2][4][96][48];              // Least count correction
  float _twr_lc_error[2][4][96][48];              // Least count correction

  int   _twr_sl_stat[2][4][96][48];         // Slewing correction status
  float _twr_sl[2][4][96][48];              // Slewing correction
  float _twr_sl_error[2][4][96][48];              // Slewing
						  // correction
  ClassDef(PdbEmcT0Tower,1);

};

#endif /* __PDBEMCT0TOWER_HH__ */
