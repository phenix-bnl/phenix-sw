#ifndef __MEMCTOFCORR6MODULE_H__
#define __MEMCTOFCORR6MODULE_H__
//-----------------------------------------------------------------------------
//
//
// Purpose: wraps the TOF Run-by-Run & Tower-by-Tower correction of DST
// 
// Description: algorithm class
//              
// The application of this class will make your life better, will make you smarter,
// and your hair grow fuller *)

// *) individual results may vary.
// $Author: pinkenbu $
// $Date: 2006/01/14 04:34:45 $
// $Name:  $
// $Source: /afs/rhic.bnl.gov/phenix/PHENIX_CVS/offline/packages/emc/mEmcTOFCorr6Module.h,v $

//-----------------------------------------------------------------------------

#include "phool.h"
#include "PdbEmcT0Sector.hh"
#include "PdbEmcT0Tower.hh"

/** EMCAL TOF run-by-run and tower-by-tower corrections, version 6.
 */
class PHCompositeNode;

class mEmcTOFCorr6Module
{

private:
  mEmcTOFCorr6Module();

 public:
  virtual ~mEmcTOFCorr6Module(){}

  /// Factory
  static mEmcTOFCorr6Module* instance();

  /// Accessor to the real data base

  void readDataFromDB(const int run_number);

  /// required by PHOOL
  PHBoolean  event(PHCompositeNode * root) ;
  PHBoolean  eventFirst(PHCompositeNode * root) ;

 private:

  /// Internal functionss
  float get_correction( int arm, int sec, int ind_y, int ind_z );

  PdbEmcT0Sector TheSector;
  PdbEmcT0Tower TheTower;
  PdbEmcT0Sector TheSectorVd;

  int isvalid;

};


#endif /* __MEMCTOFCORR6MODULE_H__ */
