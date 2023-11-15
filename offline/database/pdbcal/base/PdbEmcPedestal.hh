//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbEmcPedestal
//
//  Purpose: Stores AMU cell dependant pedestals values for one EMCAL channel.
//
//  Description: There are 3(High,Low,TAC)x64(AMUs) values per channel.
//
//  Author: L. Aphecetche
//
//-----------------------------------------------------------------------------
#ifndef __PDBEMCPEDESTAL_HH__
#define __PDBEMCPEDESTAL_HH__

#include "PdbCalChan.hh"

/** Store AMU cell dependent pedestals value for one channel. */

class PdbEmcPedestal : public PdbCalChan {
public:
  PdbEmcPedestal() ;

  /// Set the pedestal valueS (LGPP,HGPP,TAC) for one amu_cell
  void Set(int amu_cell, int low, int high, int tac) ;
  /// Get the values back
  void GetValues(int amu_cell, int& low, int& high, int& tac) ;
  virtual ~PdbEmcPedestal() ;
  
  virtual void print() const ;

private:
  int fLow[64] ;
  int fHigh[64] ;
  int fTAC[64] ;

  ClassDef(PdbEmcPedestal,1);
};

#endif /* __PDBEMCPEDESTAL_HH__ */
