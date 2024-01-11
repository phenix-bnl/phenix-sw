//-----------------------------------------------------------------------------
//
//  (c) PHENIX Collaboration 2000/10/15
//
// Purpose: wraps the MIP correction of DST
// 
// Description: algorithm class
//              
// Author:  Hisayuki Torii
//
//-----------------------------------------------------------------------------
#ifndef __MEMCMIPCORRMODULE_H__
#define __MEMCMIPCORRMODULE_H__
#include "phool.h"

/** (STAF,OLD) Wraps the MIP correction of DST.
@ingroup staf
@ingroup deprecated
*/

class PHCompositeNode;

class mEmcMIPCorrModule {
 public:
  /// default ctor
  mEmcMIPCorrModule();
  /// dtor
  virtual ~mEmcMIPCorrModule() {} ;

  /// Factory
  static mEmcMIPCorrModule* instance(char* filename);
  static mEmcMIPCorrModule* instance();
  /// required by PHOOL (kept empty)
  PHBoolean  event(PHCompositeNode * root) ;
  // Access file DB
  void readfile(char* filename);
  void print();

 private:
  static mEmcMIPCorrModule* _instance;
  int   fVerbose ;
  float _corrfact[8][96][48];        //Sector*Zrow*Yrow
  float _corrfact_err[8][96][48];    //     For PbGl 2*96*48
  float _corrfact_stat[8][96][48];   //     For PbSc 6*72*36
};
#endif /*__MEMCMIPCORRMODULE_H__*/



