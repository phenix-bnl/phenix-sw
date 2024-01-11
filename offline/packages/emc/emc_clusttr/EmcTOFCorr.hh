//-----------------------------------------------------------------------------
//
//  (c) PHENIX Collaboration 2000
//
// Purpose: wraps the TOF Run-by-Run & Tower-by-Tower correction of DST
// 
// Description: algorithm class
//              
// Author:  Ken Oyama
//          H.Torii
//-----------------------------------------------------------------------------
#ifndef __MEMCTOFCORRMODULE_H__
#define __MEMCTOFCORRMODULE_H__


#define MEMCTOFCORR_DEF_RBR_CORR 0.00
#define MEMCTOFCORR_DEF_TBT_CORR 0.00
#define MEMCTOFCORR_N_MAX_RUNS   10000

class EmcTOFCorr {
 public:

  /// default ctor
  EmcTOFCorr();
  /// dtor
  ~EmcTOFCorr();

  /// Factory
  static EmcTOFCorr* instance( char *pbsc_file, char *pbgl_file, char *tbt_file );
  static EmcTOFCorr* instance();

  float get_correction( int run, int arm, int sec, int ind_y, int ind_z );

 private:
  static EmcTOFCorr* _instance;
  void   read_rbrsc_file( char *file);
  void   read_rbrgl_file( char *file);
  void  read_tbt_file( char *file );

  int    fVerbose ;

  int    _pbsc_runs[MEMCTOFCORR_N_MAX_RUNS];   // Existing run number list of tof run-by-run correction.
  float  _pbsc_corr[MEMCTOFCORR_N_MAX_RUNS];   // Corresponding run-by-run correction.
  int    _pbsc_nruns;                          // Number of column in the rbr table.
  int    _pbgl_runs[MEMCTOFCORR_N_MAX_RUNS];   // Some things for PbGl
  float  _pbgl_corr[MEMCTOFCORR_N_MAX_RUNS];
  int    _pbgl_nruns;
  float  _tbt_corr[2][4][48][96];              // Tower-by-tower correction. Indexes are [ARM][SEC][Y][Z]
};

#endif /*__MEMCTOFCORRMODULE_H__*/

// EOF
