//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999-2001
//
//  Declaration of class PdbEmcPedestal5
//
//  Purpose: Stores AMU cell dependant pedestals values for one EMCAL channel.
//
//  Description: There are 5(Pre-High,Pre-Low,Post-High,Post-Low,TAC)x64(AMUs)
//               values per channel.
//
//  Author: L. Aphecetche
//
//-----------------------------------------------------------------------------
#ifndef __PDBEMCPEDESTAL5_HH__
#define __PDBEMCPEDESTAL5_HH__

#include "PdbCalChan.hh"

/** Store AMU cell dependent pedestals values for one channel. */

class PdbEmcPedestal5 : public PdbCalChan {
public:
  PdbEmcPedestal5() ;

  /** Set the pedestal valueS (HG_Pre,HG_Post,LG_Pre,LG_Post,TAC) for one 
      amu_cell. */
  void Set(int amu_cell, int hg_pre, int hg_post, 
	   int lg_pre, int lg_post, int tac) ;
  /// Get the values back
  void GetValues(int amu_cell, int& hg_pre, int& hg_post, 
	   int& lg_pre, int& lg_post, int& tac) ;
  virtual ~PdbEmcPedestal5() ;
  
  virtual void print() const ;

private:
  int fHG_Pre[64] ;
  int fHG_Post[64] ;
  int fLG_Post[64] ;
  int fLG_Pre[64] ;
  int fTAC[64] ;

  ClassDef(PdbEmcPedestal5,1);
};

#endif /* __PDBEMCPEDESTAL5_HH__ */
