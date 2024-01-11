//-----------------------------------------------------------------------------
//
//  (c) PHENIX Collaboration 2000
//
// Purpose: wraps the TOF Run-by-Run & Tower-by-Tower correction of DST
// 
// Description: algorithm class
//              
// Author:  Ken Oyama
//          Modified by Hisayuki Torii Dec 2000
//          Modified for V05 production by H.Torii Aug/15/2001
//
/* ======================================
   Memo by H.Torii
   
   TOF correction done by the following function.
   
   TOFCORR =
   _tbt_lc[arm][sec][y][z] * tof  - _tbt_corr[arm][sec][y][z] - _pb**_corr[run]
   
   --------HOW to use---------
   mEmcTOFCorr2Module* mEmcTOFCorr = 
    mEmcTOFCorr2Module::instance("./t0_run_pbsc_merge","./t0_run_pbgl_merge","./t0_tower_merge");
   while( event ){
    mEmcTOFCorr->event(topNode);
    //Analysis ...
   }
   =========================================*/
//-----------------------------------------------------------------------------
#ifndef __MEMCTOFCORR2MODULE_H__
#define __MEMCTOFCORR2MODULE_H__

#include "phool.h"

#include "TString.h"

#include <iostream>

class PHCompositeNode;

#define MEMCTOFCORR_DEF_RBR_CORR 0.00
#define MEMCTOFCORR_DEF_TBT_CORR 0.00
#define MEMCTOFCORR_N_MAX_RUNS   10000

/** (OLD) EMCAL TOF run-by-run and tower-by-tower corrections, version 2.
 */

class mEmcTOFCorr2Module
{
 public:

  /// default ctor
  mEmcTOFCorr2Module();
  /// dtor
  virtual ~mEmcTOFCorr2Module(){}

  /// Factory
  static mEmcTOFCorr2Module* instance( char *pbsc_file, char *pbgl_file, char *tbt_file );
  static mEmcTOFCorr2Module* instance();
  void   read_rbrsc_file( char *file);
  void   read_rbrgl_file( char *file);
  void   read_tbt_file( char *file );
  void   read_tbt_lc_file( char *file ) { std::cout << "read_tbt_lc_file not implemented" << std::endl; }
  void   read_t0_file( char *pbsc_file, char *pbgl_file, char *tbt_file );
  void   read_tdcped_file(char *file ){ tdcped_file = file; };

  /// required by PHOOL (kept empty)
  PHBoolean  event(PHCompositeNode * root) ;
  PHBoolean  eventFirst(PHCompositeNode * root) ;

 private:
  static mEmcTOFCorr2Module* _instance;
  float get_correction( int run, int arm, int sec, int ind_y, int ind_z );
  TString tdcped_file;
  PHBoolean apply_tdcped(int runNum,PHCompositeNode* topNode);

  int    fVerbose ;

  int    _pbsc_runs[MEMCTOFCORR_N_MAX_RUNS];   // Existing run number list of tof run-by-run correction.
  float  _pbsc_corr[MEMCTOFCORR_N_MAX_RUNS];   // Corresponding run-by-run correction.
  int    _pbsc_nruns;                          // Number of column in the rbr table.
  int    _pbgl_runs[MEMCTOFCORR_N_MAX_RUNS];   // Some things for PbGl
  float  _pbgl_corr[MEMCTOFCORR_N_MAX_RUNS];
  int    _pbgl_nruns;
  float  _tbt_corr[2][4][48][96];              // Tower-by-tower correction. Indexes are [ARM][SEC][Y][Z]
  float  _tbt_lc[2][4][48][96];                // Tower-by-tower least count correction.
};

#endif /*__MEMCTOFCORRMODULE_H__*/

// EOF
