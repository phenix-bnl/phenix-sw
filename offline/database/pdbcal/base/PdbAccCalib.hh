//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbAccCalib
//
//  Purpose: User defined storage class
//
//  Description:
//
//  Author: masui
//-----------------------------------------------------------------------------
#ifndef __PDBACCCALIB_HH__
#define __PDBACCCALIB_HH__

#include "PdbCalChan.hh"

class PdbAccCalib : public PdbCalChan {

public:
  PdbAccCalib();
  virtual ~PdbAccCalib();

  virtual void print() const;

  int get_status() const {return status;}
  float get_calibpar(const int ival) const {return calibpar[ival];}

  void set_status(const int ival) {status = ival;}
  void set_calibpar(const int ival, const float val) {calibpar[ival] = val;}

private:

  static const int npar = 4;
  int status;           // status of calibration parameter
  float calibpar[npar]; // calibration parameter

  ClassDef(PdbAccCalib,1);
};

#endif /* __PDBACCCALIB_HH__ */
