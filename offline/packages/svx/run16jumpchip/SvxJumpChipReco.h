#ifndef __SVXJUMPCHIPRECO_H__
#define __SVXJUMPCHIPRECO_H__

#include <Rtypes.h>
#include <SubsysReco.h>
#include <CorrDataV1.h>

#include <stdint.h>

class PHCompositeNode;
class Event;
class TGraph;
class TFile;
class TTree;

class SvxJumpChipReco: public SubsysReco
{
 public:
  SvxJumpChipReco(const std::string &name = "SVXJUMPCHIPRECO");
  virtual ~SvxJumpChipReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int EndRun(const int runno);

  /// Count the number of active bits in v
  uint32_t HammingWeight(uint32_t v);

 protected:
  void GetSiliconSums(PHCompositeNode *topNode);

  Int_t    _verbose;
  Int_t    _EventNumber;  // eventnumber for event being processed
  Float_t  _bbc_qsum;
  UInt_t   _strig;        // scaled trigger word
  UInt_t   _cross;        // clock crossing
  ULong64_t _clock;       // 64 bit beam clock counter

  static const int NPACKETS = 3;
  static const int NCHIPS = 8;
  static const int NEVENTS = 100;   // num events in window to look at correlation

  TFile   *savefile;

  TGraph  *_bbcq_vs_chiphits[NPACKETS][NCHIPS];
  TGraph  *_corr_vs_event[NPACKETS][NCHIPS]; // correlation strength between bbcq and hits in a chip

  TTree     *ttree;
  Int_t     fEvent;
  UInt_t    fStrig;
  Int_t   fVTXPsum;
  Int_t   fVTXSsum;
  Int_t   fFVTXsum;
  ULong64_t fCross;
  ULong64_t fClock;
  CorrData  *fCorrdata;

};

#endif /* __SVXJUMPCHIPRECO_H__ */

