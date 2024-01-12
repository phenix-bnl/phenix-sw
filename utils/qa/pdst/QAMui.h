/*
 * QAMui.h
 * $Id: QAMui.h,v 1.2 2008/06/26 13:59:40 hpereira Exp $
 */

#ifndef _QAMui_H
#define _QAMui_H

#include <TH1.h>
#include <TH2.h>
#include "SubsysReco.h"

class PHCompositeNode;
class TriggerHelper;
class TMuiHVTable;


class QAMui: public SubsysReco
{
 public:
  
  //! constructor
  QAMui(const char *name = "QAMui"): 
    SubsysReco(name),
    _has_trigger( false ),
    _hv_map( 0 )
  {}
  
  //! destructor
  virtual ~QAMui() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

  //! number of triggers
  enum { 
    N_ARMS = 3,
    N_TRIGGERS = 6 
  };

  protected:
  
  //! hit histograms
  void fill_mui_hits_hists( PHCompositeNode* top_node, TriggerHelper& fTrigHelper );
  
  //! track histograms
  void fill_mui_tracks_hists( PHCompositeNode* top_node, TriggerHelper& fTrigHelper );
  
  //! road histograms
  void fill_mui_roads_hists( PHCompositeNode* top_node, TriggerHelper& fTrigHelper );

  private:
  
  //! true when level1 trigger are found
  bool _has_trigger;
  
  //! HV table
  TMuiHVTable* _hv_map;

  // event/trigger level histograms
  TH1F *muiRunNumber;
  TH1F *muiDataType;
  TH1F *muiMinBias;
  TH1F *muiTriggerBit;
  TH1F *muiTriggerBit_scaled;
  TH1F *muiTriggerBit_live;
  
  // histograms: for each trigger 
  // 1-dim
  TH1F *muiNHits[N_TRIGGERS];
  TH1F *muiNRoads[N_TRIGGERS];
  TH1F *muiNRoads_NoGhost[N_TRIGGERS];
  TH1F *muiNRoads_Golden[N_TRIGGERS];
  TH1F *muiNGolden_per_Road[N_TRIGGERS];
  TH1F *muiRoadTrack_MatchPos[N_TRIGGERS];
  TH1F *muiLastPlane[N_TRIGGERS];
  TH1F *muiMaxHits[N_TRIGGERS];
  TH1F *muiFitQuality[N_TRIGGERS];
  TH1F *muiBBCZVertex[N_TRIGGERS];
  TH1F *muiChainHits[N_TRIGGERS];
  // 2-dim
  TH2F *muiRoadGap0[N_TRIGGERS];
  TH2F *muiRoadRefPos[N_TRIGGERS];
  TH2F *muiP_Depth[N_TRIGGERS];
  TH2F *muiNHits_BBCch[N_TRIGGERS];
  
};

#endif /*_QAMui_H*/

//EOF
