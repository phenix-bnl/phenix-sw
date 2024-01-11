#ifndef __MEMCTOFCORR4MODULE_H__
#define __MEMCTOFCORR4MODULE_H__

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

//-----------------------------------------------------------------------------

#include "phool.h"
#include "PdbEmcT0Sector.hh"
#include "PdbEmcT0Tower.hh"

/** (OLD) EMCAL TOF run-by-run and tower-by-tower corrections, version 4
 */

class PHCompositeNode;

class mEmcTOFCorr4Module
{

private:
  mEmcTOFCorr4Module();

 public:
  virtual ~mEmcTOFCorr4Module(){}

  /// Factory
  static mEmcTOFCorr4Module* instance();

  /// Accessor to the real data base

  void readDataFromDB(const int run_number);

  /// required by PHOOL
  PHBoolean  event(PHCompositeNode * root) ;
  PHBoolean  eventFirst(PHCompositeNode * root) ;

 private:
  /// Factory
  static mEmcTOFCorr4Module* _instance;

  /// Internal functionss
  float get_correction( int arm, int sec, int ind_y, int ind_z );

  PdbEmcT0Sector TheSector;
  PdbEmcT0Tower TheTower;

  int isvalid;

};


#endif /* __MEMCTOFCORR4MODULE_H__ */
