#ifndef __MEMCTOFCORR5MODULE_H__
#define __MEMCTOFCORR5MODULE_H__
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
#include <SubsysReco.h>
#include "PdbEmcT0Sector.hh"
#include "PdbEmcT0Tower.hh"

/** (OLD) EMCAL TOF run-by-run and tower-by-tower corrections, version 5.
 */

class PHCompositeNode;

class mEmcTOFCorr5Module: public SubsysReco {

private:
  mEmcTOFCorr5Module();

 public:
  virtual ~mEmcTOFCorr5Module(){}

  /// Factory
  static mEmcTOFCorr5Module* instance();

  /// Accessor to the real data base

  void readDataFromDB(const int run_number);

  /// required by PHOOL
  int process_event(PHCompositeNode * root) ;


 private:

  /// Internal functionss
  float get_correction( int arm, int sec, int ind_y, int ind_z );

  PdbEmcT0Sector TheSector;
  PdbEmcT0Tower TheTower;
  PdbEmcT0Sector TheSectorVd;

  int isvalid;

};


#endif /* __MEMCTOFCORR4MODULE_H__ */
