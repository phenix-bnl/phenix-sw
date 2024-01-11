#ifndef __ALPHAPHI_H__
#define __ALPHAPHI_H__

#include <SubsysReco.h>

class PHCompositeNode;
class Fun4AllHistoManager;
class PHCentralTrack;   
class DchTrack;
class DchHitLineTable;
class PHGlobal;
class TH1;
class TH2;

class DchAlphaPhi: public SubsysReco
{

 public:
  DchAlphaPhi(Fun4AllHistoManager *HM);
  virtual ~DchAlphaPhi() {};

  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  int wireSide(int iTrack, short start, short end);
  int sideNumber(int iTrack);
  int cardNumber(int iTrack, short start, short end);
  int findNodes(PHCompositeNode *topNode);  
  
 protected:
  Fun4AllHistoManager *hm;
  PHCentralTrack  *d_cnt;
  DchTrack *d_dch;
  DchHitLineTable *d_hit;
  PHGlobal *global;

  //  Status histogram
  TH1 *Status;
  // PHCentralTrack alpha v phi
  TH2 *PHAlphaPhiAll;
  TH2 *PHAlphaPhiEast;
  TH2 *PHAlphaPhiWest;
  //  Histograms to determine *our* calibration...
  TH2 *Alpha1East;
  TH2 *Alpha2East;
  TH2 *Alpha1West;
  TH2 *Alpha2West;
  // Same histograms by sides (VGR)
  TH2 *Alpha1Side0East;
  TH2 *Alpha2Side0East;
  TH2 *Alpha1Side0West;
  TH2 *Alpha2Side0West;
  TH2 *Alpha1Side1East;
  TH2 *Alpha2Side1East;
  TH2 *Alpha1Side1West;
  TH2 *Alpha2Side1West;
  // Same histograms by sides and parity (even<->odd) (VGR)
  TH2 *Alpha1Side0EvenEast;
  TH2 *Alpha2Side0EvenEast;
  TH2 *Alpha1Side0EvenWest;
  TH2 *Alpha2Side0EvenWest;
  TH2 *Alpha1Side1EvenEast;
  TH2 *Alpha2Side1EvenEast;
  TH2 *Alpha1Side1EvenWest;
  TH2 *Alpha2Side1EvenWest;
  TH2 *Alpha1Side0OddEast;
  TH2 *Alpha2Side0OddEast;
  TH2 *Alpha1Side0OddWest;
  TH2 *Alpha2Side0OddWest;
  TH2 *Alpha1Side1OddEast;
  TH2 *Alpha2Side1OddEast;
  TH2 *Alpha1Side1OddWest;
  TH2 *Alpha2Side1OddWest;
  //Alpha vs cell (VGR) 
  TH2 *AlphaCellEast;
  TH2 *AlphaCellWest;
  // Histograms to determine whether centering is good
  TH2 *DCAlphaPhiEast;
  TH2 *DCAlphaPhiWest;
  //Same histograms only Alpha vs phi for X1&X2 (VGR)
  TH2 *Alpha1PhiEast;
  TH2 *Alpha2PhiEast;
  TH2 *Alpha12PhiEast;
  TH2 *Alpha1PhiWest;
  TH2 *Alpha2PhiWest;
  TH2 *Alpha12PhiWest;
  //Info histograms, give phi vs cell (VGR)
  TH2 *CellPhiEast;
  TH2 *CellPhiWest;

  TH1 *HitPhi;
  TH2 *Image;

};

#endif /*__ALPHAPHI_H__ */
  
