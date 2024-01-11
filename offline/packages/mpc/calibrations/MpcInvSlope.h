#ifndef __MPCINVSLOPE_H__
#define __MPCINVSLOPE_H__

#include <SubsysReco.h>
#include <string>
#include <Fun4AllReturnCodes.h>

class PHCompositeNode;
class TriggerHelper;
class TFile;
class TH1;
class TH2;
class MpcMap;
class MpcCalib;


class MpcInvSlope: public SubsysReco
{
public:
  MpcInvSlope(const char* outfile = "mpcinvslope.root");
  virtual ~MpcInvSlope() {}

  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);

  int IsSingleTower(const int fee576_ch);

protected:

  MpcMap   *mpcmap;
  MpcCalib *mpccalib;
  TriggerHelper *trighelp;

  Int_t EventCounter;

  std::string OutFileName;
  TFile *savefile;

  TH1 *h_lo[576];	// lo gain adc
  TH1 *h_hi[576];	// hi gain adc
  TH1 *h_e[576];	// corrected energy
  TH1 *h_lopt[576];	// lo gain adc
  TH1 *h_hipt[576];	// hi gain adc
  TH1 *h_et[576];	// corrected energy

  TH2 *h2_ecent[2];	// ecent for each tower
  TH2 *h2_e9[2];	// e9

  TH1 *hzvtx;		// zvertex
  TH1 *hzvtxcut;	// zvertex after analysis cut
  TH1 *hsample;		// sample number
};

#endif /* __MPCINVSLOPE_H__ */


