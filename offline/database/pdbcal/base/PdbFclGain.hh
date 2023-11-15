//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbFclGain
//
//  Purpose: User defined storage class
//
//  Description:
//
//  Author: mheffner
//-----------------------------------------------------------------------------
#ifndef __PDBFCLGAIN_HH__
#define __PDBFCLGAIN_HH__

#include "PdbCalChan.hh"

class PdbFclGain : public PdbCalChan {
public:
  PdbFclGain();
  virtual ~PdbFclGain();
  PdbFclGain& operator = (const PdbFclGain &p);

  virtual void print() const;

  virtual int setNorthGain(const int row,
			   const int col,
			   const double value,
			   const double error);
  virtual int setSouthGain(const int row,
			   const int col,
			   const double value,
			   const double error);
  virtual double getNorthGain(const int row,
			      const int col) const;
  virtual double getSouthGain(const int row,
			      const int col) const;
  virtual double getNorthGainError(const int row,
				   const int col) const;
  virtual double getSouthGainError(const int row,
				   const int col) const;
  
private:
  double northGain[10][9];
  double southGain[10][9];
  double northGainError[10][9];
  double southGainError[10][9];

  ClassDef(PdbFclGain,1);
};

#endif /* __PDBFCLGAIN_HH__ */
