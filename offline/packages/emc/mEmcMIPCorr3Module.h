//-----------------------------------------------------------------------------
//
//  (c) PHENIX Collaboration 2000/10/15
//
// Purpose: wraps the MIP correction of DST for Run2
// 
// Description: algorithm class
//              
// Author:  Hisayuki Torii
//          Modified for Run2V02 production by H.Torii Jan/30/2002
//
//-----------------------------------------------------------------------------
#ifndef __MEMCMIPCorr3MODULE_H__
#define __MEMCMIPCorr3MODULE_H__
#include "phool.h"
#include <vector>

class PHCompositeNode;

/** (STAF) Mip correction for Run2 dsts. 
@ingroup staf
@ingroup deprecated
*/

class mEmcMIPCorr3Module
{
 public:
  /// default ctor
  mEmcMIPCorr3Module();
  /// dtor
  virtual ~mEmcMIPCorr3Module() {} ;

  /// Factory
  static mEmcMIPCorr3Module* instance();

  /// required by PHOOL (kept empty)
  PHBoolean  event(PHCompositeNode * root) ;

  // Access file DB
  void readfile_twr(char* filename);
  void readfile_run(char* filename);
  void print();
  float get_corr_run(int iarm,int isect, int run);
  float get_corr_twr_mip(int arm, int sect, int iz, int iy) const { return _corr_twr_mip[arm][sect][iz][iy];}

 private:
  static mEmcMIPCorr3Module* _instance;
  int   fVerbose ;
#ifndef __CINT__
  std::vector<int> _corr_run[2][4];           //! for 8 Sector
  std::vector<float> _corr_run_mip[2][4];     //! for 8 Sector
  std::vector<float> _corr_run_err[2][4];     //! for 8 Sector
#endif
  int _last_run[2][4];
  float _last_run_mip[2][4];

  float _corr_twr_mip[2][4][96][48];        //Sector*Zrow*Yrow
  float _corr_twr_err[2][4][96][48];    //     For PbGl 2*96*48
  bool _corr_twr_stat[2][4][96][48];   //     For PbSc 6*72*36
};
#endif /*__MEMCMIPCorr3Module_H__*/



