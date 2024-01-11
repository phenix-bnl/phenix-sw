#ifndef __MPCTRIGEMULATOR_H__
#define __MPCTRIGEMULATOR_H__

#include <SubsysReco.h>
#include <stdint.h>

class PHCompositeNode;
class mpcSampleContainer;

class MpcTrigEmulator: public SubsysReco
{
public:
  MpcTrigEmulator(const std::string &name = "MpcTrigEmulator");
  virtual ~MpcTrigEmulator();

  //int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int EndRun(const int runno);

  int  CalcSumTrigger( mpcSampleContainer *samples );

  //
  // n[3], s[3] = A, B, C inclusive decision
  // a[12], b[12], c[12] are the module decisions
  //
  void TriggerDecision(int *n, int *s, int *a, int *b, int *c);

  int GetModuleTrig(const int itrig, const int module) {
    int trig_fired = -1;
    trig_fired = fTrig[itrig][module];
    return trig_fired;
  }

  uint32_t GetModuleSum(const int module) {
    return total_sum[module];
  }

  uint32_t GetModuleNhits(const int module) {
    return total_nhits[module];
  }
  
  void SetPtTrigger();
  int  SetSumTrigger();

  void SetTrigChThreshold(const int ch, const int32_t t);
 
  void SetTrigScaleFactor(const int ch, const uint32_t s) {
    trig_scalefactor[ch] = s;
  }
 
  void SetAdcSumThreshold(const int itrig, const uint32_t t) {
    trig_adcsum_threshold[itrig] = t;
  }
 
  void SetNhitThreshold(const int itrig, const uint32_t t) {
    trig_nhit_threshold[itrig] = t;
  }
 
  void SetPrePostDelay(const int32_t d) {
    trig_prepost_delay = d;
  }

  uint32_t GetTrigScaleFactor(const int ch) {
    return trig_scalefactor[ch];
  }
 
protected:
  //MpcMap *mpcmap;

  static const int NMODULES = 12;
  static const int NTRIGS = 3;		// A,B,C
  static const int NUMCH = 576;
  int fTrig[NTRIGS][NMODULES];

  uint32_t trig_scalefactor[NUMCH];
  int32_t trig_ch_threshold[NUMCH];
  uint32_t trig_adcsum_threshold[NTRIGS];
  uint32_t trig_nhit_threshold[NTRIGS];
  int32_t trig_prepost_delay;

  uint32_t total_sum[12];			// sum in each of 12 modules
  uint32_t total_nhits[12];		// nhits in each of 12 modules

};

#endif /* __MPCTRIGEMULATOR_H__ */

