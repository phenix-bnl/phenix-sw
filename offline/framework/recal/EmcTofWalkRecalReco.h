#ifndef __EMCTOFWALKRECALRECO_H__
#define __EMCTOFWALKRECALRECO_H__

#include "emcManageable.h"
#include <emcClusterContent.h>
#include <Recalibrator.h>
#include <set>
#include <string>
#include <vector>

using namespace std;

/** EmcTofWalkRecalReco is a replacement of the good old [...]Emctofrecalreco classes 
 *  for run7 but it is based on standard calibration methods.
 *
 *  Possible ways of usage
 *  (choice hardwired, should only depend on run#)
 *
 *  "Direct" recalibration:
 *  It will use the newest valid _production_ Pg values and the raw ADC & TDC.
 *  Warning: because of its need for raw values, it can't work with older versions
 *  than emcClusterContainerv5; and so will it crash on a PhPhotonList!
 *  If there is no raw ADC value in the container (that is present from v6), or if demanded,
 *  the ADC value is restored from the gains. 
 *
 *  "Afterburn"
 *  Will UNDO production calibration and then REDO it with a specified set of 
 *  calibrations. In principle it'll work on any emcClusterContainer that has the
 *  ToF fields.
 *  
 *  "Single-Afterburn"
 *  Same as above only for walk, but with only one calibration dataset.
 *  t0 is recalibrated by adding up new shifts (no undo)
 *  old wk (for undo) is from WalkTofs value0, new wk (for redo) is from WalkTofs value1
 * 
 *  R.Vertesi (vertesi@bnl.gov)
 */

class PHCompositeNode;
class emcCalibrationDataHelper;

class EmcTofWalkRecalReco : public Recalibrator
{
 public:

  /// ctor.
  EmcTofWalkRecalReco( const std::string &name = "EmcTofWalkRecalReco");
  virtual ~EmcTofWalkRecalReco(){}
  
  int InitRun(PHCompositeNode *topNode);
  int EndRun(const int runnumber);
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

  void setLoadPbScWalks(bool doit = true) { fpbscwalk = doit ; }
  void setLoadPbScTowerOffsets(bool doit = true) { fpbsctwr = doit; }
  void setLoadPbScLeastCounts (bool doit = true) { fpbsclc = doit; }
  void setLoadPbSc(bool doit = true) { 
    setLoadPbScWalks(doit);
    setLoadPbScTowerOffsets(doit); 
    setLoadPbScLeastCounts(doit);
  }
  void setLoadPbGlWalks(bool doit = true) { fpbglwalk = doit ; }
  void setLoadPbGlTowerOffsets(bool doit = true) { fpbgltwr = doit; }
  void setLoadPbGlLeastCounts (bool doit = true) { fpbgllc = doit; }
  void setLoadPbGl(bool doit = true) { 
    setLoadPbGlWalks(doit);
    setLoadPbGlTowerOffsets(doit); 
    setLoadPbGlLeastCounts(doit);
  }
  void setLoadSectorOffsets(bool doit = true) { fdosec = doit; }

  void setUseDirectADC(bool doit = 0) { fdirectadc = doit; }
  void setUseAfterburn(int doit = 1) { fafterburn = doit; 
    /** 0=Direct, 1=Afterburn, 2=Single afterburn */ }

  void Deactivate();

  void setSourceTofT0Bs(emcManageable::EStorage src,
			emcManageable::EStorage oldsrc = emcManageable::kNone )
  { fsrcTofT0Bs = src; fsrcOldTofT0Bs = oldsrc; } 
  void setSourceWalkTofs(emcManageable::EStorage src,  
 			 emcManageable::EStorage oldsrc =  emcManageable::kNone)  
  { fsrcWalkTofs = src; fsrcOldWalkTofs = oldsrc; }
  void setSourceSectorOffsets(emcManageable::EStorage src, 
			      emcManageable::EStorage oldsrc =  emcManageable::kNone )
  { fsrcSectorTofs = src; fsrcOldSectorTofs = oldsrc; } 
  void CacheConstants(int);
  void CacheOldConstants(int);
  void CacheGainConstants(int);
  
  void SetClusterDeltaT(emcClusterContent*, float, float);

 private:
  float calibrateTime(const int, const float, const float, const float, const float, const float);
  float flashTime(const float, const float, const float, const float);
  float restoreADC(const int, const float);

  static const int ntowers = 24768;
  static const int nsectors = 8;

  std::vector<float> flc, fgain, fnormt;
  std::vector<float> fwk, ftwroff, fsecoff;
  std::vector<float> fOwk, fOtwroff, fOsecoff;

  emcManageable::EStorage fsrcTofT0Bs,   fsrcOldTofT0Bs;
  emcManageable::EStorage fsrcWalkTofs,  fsrcOldWalkTofs;
  emcManageable::EStorage fsrcSectorTofs,fsrcOldSectorTofs;
  bool fpbscwalk, fpbsctwr, fpbsclc;
  bool fpbglwalk, fpbgltwr, fpbgllc;
  bool fdosec;
  bool fdirectadc;
  int fafterburn;
  bool fdeactivated;

};



#endif /* __EMCTOFWALKRECALRECO_H__ */
