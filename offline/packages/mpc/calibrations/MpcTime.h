#ifndef __MPCTIME_H__
#define __MPCTIME_H__

#include <SubsysReco.h>
#include <string>
#include <Fun4AllReturnCodes.h>

class PHCompositeNode;
class TriggerHelper;
class TFile;
class TTree;
class TH1;
class TH2;
class MpcMap;
class MpcCalib;


class MpcTime: public SubsysReco
{
public:
  MpcTime(const char* outfile = "mpcinvslope.root");
  virtual ~MpcTime() {}

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
  TTree *tree;
  Int_t   f_ch;		// feech
  Float_t f_lo;		// lopost-lopre
  Float_t f_hi;		// hipost-hipre
  Float_t f_e;		// tower energy
  Float_t f_tdc;	// tdc
  Float_t f_tdc2ns;	// tdc 2 ns conversion
  Float_t f_t0offset;	// tdc offset
  Float_t f_bbcz;	// bbc zvertex, etc
  Float_t f_bbct0;
  Float_t f_bbctn;
  Float_t f_bbcts;

  TH1 *htdiff[576];

  TH2 *h2_ecent[2];     // ecent for each tower
  TH2 *h2_e9[2];        // e9

  TH1 *hzvtx;		// zvertex
  TH1 *hzvtxcut;	// zvertex after analysis cut
  TH1 *ht0;		// time zero
  TH1 *ht0cut;		// time zero after cut?
};

#endif /* __MPCTIME_H__ */


