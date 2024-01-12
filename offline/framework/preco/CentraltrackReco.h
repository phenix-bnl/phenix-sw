#ifndef __CENTRALTRACKRECO_H__
#define __CENTRALTRACKRECO_H__

#include "SubsysReco.h"

class PHCompositeNode;
class PHCentralTrack;
class RunHeader;
class BbcOut;
class VtxOut;
class DchTrack;
class PadCluster;
class PadCluster;
class PadCluster;
class TecOut;
class emcClusterContainer;
class EmcClusterLocalExt;
class emcTowerContainer;
class TofOut;
class CglTrack;
class PHTrackOut;
class CrkRing;
class T0Out;
class Event;
class PHDchTrackOut;
class AccRaw;
class AccProj;
class HbdMiniCellList;
class HbdCellList;
class TofwHit;

class CentraltrackReco: public SubsysReco
{
 public:
  CentraltrackReco(int version = 22, const char * name = "CENTRALTRACK");
  virtual ~CentraltrackReco() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  int CreateNodeTree   (PHCompositeNode *topNode);
  int FillCommon       (PHCompositeNode *topNode);
  int FillIndices      (PHCompositeNode *topNode);
  int FillAbsolutePos  (PHCompositeNode *topNode);
  int FillAbsoluteDelta(PHCompositeNode *topNode);
  int FillLvl1         (PHCompositeNode *topNode);
  int FillPadWidths    (PHCompositeNode *topNode);
  int FillTrackModel   (PHCompositeNode *topNode);
  int FillSwapped      (PHCompositeNode *topNode);
  int FillEmcE         (PHCompositeNode *topNode);
  int FillAcc          (PHCompositeNode *topNode);
  int FillTOFPH        (PHCompositeNode *topNode);
  int FillTOFTDC       (PHCompositeNode *topNode);
  int FillTECEXTRA     (PHCompositeNode *topNode);
  int FillHbdTrack     (PHCompositeNode *topNode);
  int FillTofwTrack    (PHCompositeNode *topNode);
  int FillExtraSwap    (PHCompositeNode *topNode);

  int version;
  bool Common    ;
  bool Indices   ;
  bool AbsolutePos ;
  bool AbsoluteDelta ;
  bool Lvl1      ;
  bool PadWidths ;
  bool TrackModel;
  bool Swapped   ;
  bool Run1pp    ;
  bool EmcE      ;
  bool Acc       ;
  bool TOFPH     ;
  bool TOFTDC    ;
  bool TECEXTRA  ;
  bool HbdTrack  ;
  bool TofwTrack ;
  bool fillExtraSwap ;

  int FieldOnFlag;

  //DataNodes  -- ala Akiba...
  PHCentralTrack      *particle;     
  RunHeader           *d_runhdr;  
  BbcOut              *d_bbc;     
  VtxOut              *d_vtx;     
  DchTrack            *d_dctrk;   
  PadCluster          *d_pc1;     
  PadCluster          *d_pc2;     
  PadCluster          *d_pc3;     
  TecOut              *d_tecout;  
  emcClusterContainer *d_emc;     
  emcTowerContainer   *d_twr;     
  EmcClusterLocalExt  *d_emcOLD;
  TofOut              *d_tof;     
  CglTrack            *d_cgl;     
  PHTrackOut          *d_proj;    
  CrkRing             *d_crkring; 
  T0Out               *d_t0;      
  Event               *d_evt;     
  PHDchTrackOut       *d_model;   
  CrkRing             *d_scrkring; 
  CglTrack            *d_scgl;     
  PHTrackOut          *d_sproj;    
  AccRaw              *d_accraw;
  AccProj             *d_accproj;
  HbdCellList     *d_hbd;
  HbdMiniCellList     *d_hbdmini;
  TofwHit             *d_tofw;
};

#endif /* __CENTRALTRACKRECO_H__ */


