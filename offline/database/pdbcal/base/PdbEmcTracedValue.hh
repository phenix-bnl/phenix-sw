//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbEmcTracedValue
//
//  Purpose: One Traced Line = {time,constant,slope} for EMCAL.
//
//  Description: 
//
//  Author: L. Aphecetche
//-----------------------------------------------------------------------------
#ifndef __PDBEMCTRACEDVALUE_HH__
#define __PDBEMCTRACEDVALUE_HH__

#include "PdbCalChan.hh"

/** Used to store Traced Line = {time,constant,slope}. */

class PdbEmcTracedValue : public PdbCalChan {
public:
  PdbEmcTracedValue() ;
  /// Set the time,constant and slope for one channel.
  void Set(int channel, int thetime, float constant, float slope) ;
  /// Get the values back.
  void Get(int& channel, int& thetime, float& constant, float& slope) ;  
  virtual ~PdbEmcTracedValue() ;
  
  virtual void print() const ;

private:
  int fChannel ;
  int fTime ;
  float fConstant ;
  float fSlope ;

  ClassDef(PdbEmcTracedValue,1);
};

#endif /* __PDBEMCTRACEDVALUE_HH__ */
