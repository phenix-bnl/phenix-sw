#ifndef __PBGLSECTOR_H__
#define __PBGLSECTOR_H__

#ifndef __EMCSTATICDATA_H__
#include "EmcStaticData.h"
#endif
#ifndef __PBGLCALIBRATIONDATA_H__
#include "PbGlCalibrationData.h"
#endif
#ifndef __EMCSECTOR_H__
#include "EmcSector.h"
#endif
#ifndef __PBGLSUPERMODULE_H__
#include "PbGlSuperModule.h"
#endif
#ifndef __PBGLINDEXER_H__
#include "PbGlIndexer.h"
#endif
#include <vector>

class PHTimeStamp; 

/** PbGl implementation of EmcSector. */

class PbGlSector: public EmcSector
{
 public:

  PbGlSector(int &, PHTimeStamp* ts = 0 );

  virtual ~PbGlSector();

  ///
  virtual EmcSuperModule *getSuperModule(int SMNumber) {return SM[SMNumber];}
  ///
  virtual void GetEnergyCalibration(int i, float& c0, float& g0, float& cf);
  ///
  PbGlSuperModule * GetSM24(int iSM24) {return (PbGlSuperModule *)SM[iSM24];}
  ///
  void CorrectEnergyCalibration(const char * fname);

 private:

  void BuildFromFiles(void);
  void BuildFromDB(PHTimeStamp*);
  void Reset(void);
  void LoadNextSMData(FILE * fp, EmcSuperModule * SM24);

 protected:

  ///
  std::vector<EmcSuperModule*> SM;

  std::vector<float> fC0; 
  std::vector<float> fG0;
  std::vector<float> fCF;

private:
  static size_t fgSize;
  static size_t fgNumberOfSuperModules;
};

#endif
