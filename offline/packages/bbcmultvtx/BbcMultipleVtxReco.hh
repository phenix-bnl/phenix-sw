/*
  
  BbcMultipleVtxReco.hh
  
  Created: 2011/09/09
  Last Update: 2012/03/29
  Author: Hideyuki Oide (oide@icepp.s.u-tokyo.ac.jp)
  
  
  Description:
  
  Manually reconstruct multiple BBC vertices from BbcRaw node
  using clustering for several timing binsize.
  
  Reconstructed vertex candidates can have not only real vertices but also ghosts
  depending on the number of composed clusters in each BBC.
  
  Reconstructed vertex candidates are stored in "BbcMultipleVtx" DST node.
  BbcMultipleVtx has an array of several results of clustering,
  each corresponding to the tuning the binsize of clustering.
  Each of the result is packed into BbcMultipleVtxList.
  
  For the detail of this package see following link:
  http://www.phenix.bnl.gov/cdsagenda//askArchive.php?base=agenda&categ=a1290&id=a1290s1t9/moreinfo
  
 */

#ifndef __BBCMULTIPLEVTXRECO__
#define __BBCMULTIPLEVTXRECO__

// PHENIX
#include <SubsysReco.h>
#include <Bbc.hh>

class PHCompositeNode;
class RunHeader;
class EventHeader;
class BbcRaw;
class BbcOut;
class BbcCalib;
class BbcEvent;

class TH1;

// BbcMultipleVtx
class BbcMultipleVtx;

class BbcMultipleVtxReco : public SubsysReco {
public:
  // Subsystem
  BbcMultipleVtxReco(const std::string &name="BbcMultipleVtxReco");
  virtual ~BbcMultipleVtxReco(void);
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  
  // Control Accessors
  inline void set_t0offset(const int offset) { fT0offset = offset; }
  
private:
  // Functions for Initialization
  void SetupBbcRead();
  
  // Functions for Event Processing
  void ReconstructBbcEvent(PHCompositeNode *top_node);
  void MakeCluster(BbcEvent& bbcevent, TH1 **hbbctime, const int thr);
  void ReconstructVertex();
  void FillToDST(const int bin);
  void ClearClusterVariables();
  void DumpClusterInfo(const int bin, const int thr);
  
  // BBC PrintOut
  void PrintBbcRaw(PHCompositeNode *top_node);
  void PrintBbcOut(PHCompositeNode *top_node);
  
  // Subsystem Control Variables
  int fT0offset;
  
  static const int kNMaxClus = 20;
  static const int kNtimebin = 8;
  float timebin[kNtimebin];
  
  // Nodes
  RunHeader *fRunHeader;
  EventHeader *fEventHeader;
  BbcCalib *fBbcCalib;
  BbcRaw *fBbcRaw;
  BbcOut *fBbcOut;
 
  // Output
  BbcMultipleVtx *mvtx;
  
  // Event Processing
  int fRunNumber;
  int fEventNumber;
  float fBbcOutBbcZ;
  float fBbcOutT0;
  
  bool is_cluster_made;
  
  bool bbc_hitbit[BBC_N_PMT];
  float bbc_adc[BBC_N_PMT];
  int fBbcNhit[2];
  int fBbcNclus[2];
  float fBbcClusPos[2][kNMaxClus];
  float fBbcClusPos_rms[2][kNMaxClus];
  int fBbcClusSize[2][kNMaxClus];
  float fBbcT0_candidate[kNMaxClus*kNMaxClus];
  float fBbcVtx_candidate[kNMaxClus*kNMaxClus];

};



#endif /* __BBCMULTIPLEVTXRECO__ */
