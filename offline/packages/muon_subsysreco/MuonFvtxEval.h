#ifndef __MUONFVTXEVAL_H__
#define __MUONFVTXEVAL_H__

#include <string>
#include <MUIOO.h>

#include "MuonSubsysReco.h"

#ifndef __CINT__
#include<boost/array.hpp>
#endif

#include<PHGlobal.h>

#ifndef __CINT__
#include <Fun4AllServer.h>
#include <TMutStubMap.h>
#include <TMutTrkMap.h>
#include <TMutMCTrkMap.h>
#include <TMutClusMap.h>
#include <TMutVtxMap.h>
#include <TMutHitMap.h>
#include <TMuiHitMapO.h>
#include <TMuiRoadMapO.h>
#include <TMutEvalMap.h>
#include <TMuiEvalMap.h>
#else
class TMutStubMap;
class TMutTrkMap;
class TMutMCTrkMap;
class TMutClusMap;
class TMutVtxMap;
class TMutHitMap;
class TMuiHitMapO;
class TMuiRoadMapO;
class TMutEvalMap;
class TMuiEvalMap;
#endif

class PHCompositeNode;

class TNtuple;
class TTree;

// MUON
#ifndef __CINT__
#include <mMutEval.h>
#include <mMuiEvalO.h>
#include <PHTimeServer.h>
#endif

/*!
  \class   MuonFvtxEval
  \ingroup supermodules
  \brief
  Mutoo/Fvtx reconstruction efficiency evaluation module
  runs on simulated DSTs.
*/
class MuonFvtxEval: public MuonSubsysReco
{

  public:

  //! constructor
  MuonFvtxEval(
    const char* name = "MUONFVTXEVAL",
    const char* filename = "mutoo_fvtx_eval_ntuple.root"
  );

        //! first call initialisation
  int Init(PHCompositeNode *);

        //! first call initialisation
  int InitRun(PHCompositeNode *);

  //! event method
  int process_event(PHCompositeNode *);

  //! end of run method
  int End(PHCompositeNode *);

  //! changes ntuple filename
  void set_filename( const char* file )
  { if( file ) _filename = file; }

  // ! changes signal node name
  void SetSignalNodeName(std::string name)
  { _signalNodeName = name; }
    
  protected:

  //! retrieves all used maps from _vtx_map
  void set_interface_ptrs( PHCompositeNode*  );

  //! creates local/internal nodes
  int CreateNodeTree(PHCompositeNode *);

  void write_ntuples();
  void write_timing_ntuple();
  void write_single_reco_ntuple();

  #ifndef __CINT__

  //! get depth of a mc trk, using association to MCHits
  static int get_mc_trk_depth( TMutMCTrkMap::const_pointer mc_trk_ptr );

  //! get largest depth of road associated to tracks
  static int get_max_road_depth( TMutTrkMap::const_pointer trk_ptr );

  #endif
  
  //! event veto
  void set_event_veto()
  {_event_veto=1;}
  
  //! event veto (retrieve and reset)
  int get_event_veto_reset()
  {
    int return_val = _event_veto;
    _event_veto=0;
    return return_val;
  }
  
  //! event veto (reset only)
  void unset_event_veto()
  {_event_veto=0;}
  
  //! stores/cumulate maps stats at end of event
  void store_muid_stat(PHCompositeNode* );

  //! print event stat at end of run
  void print_muid_stat( std::ostream& out = std::cout );

  //! calculate rapidity
  double rapidity(double px, double py, double pz, double mass);

  //! calculate transverse momentum
  double pt(double px, double py);

  //! initialize all ntuples
  bool initialize_ntuples();

  #ifndef __CINT__

  //!@name reconstruction modules
  //@{

  //! associate TMutTrk to TMutMCTracks
  mMutEval _mMutEval_mod;

  //! associate TMuiRoadO to TMutMCTracks
  mMuiEvalO _mMuiEval_mod;

  //@}

  //! module timer
  PHTimeServer::timer _timer;

  #endif

  // top node
  PHCompositeNode* _top_node;

  //! MUON nodes
  PHCompositeNode* _mutoo_node;

  //! MUON nodes
  PHCompositeNode* _muioo_node;

  //! DST node
  PHCompositeNode* _dst_node;

  // Nodes for input signal
  //! signal node for MC DST
  std::string _signalNodeName;
  PHCompositeNode* _signal_top_node;


  // local pointers to mutoo maps
  TMutStubMap* _stub_map;
  TMutTrkMap* _trk_map;
  TMutMCTrkMap* _mc_trk_map;
  TMutClusMap* _clus_map;
  TMutVtxMap* _vtx_map;
  TMutHitMap* _hit_map;
  TMuiHitMapO* _mui_hit_map;
  TMuiRoadMapO* _mui_road_map;
  TMutEvalMap* _eval_map;
  TMuiEvalMap* _mui_eval_map;

  //! ntuple output file name
  std::string _filename;

  //! time tree
  TTree* _timing;

  //! muon reconstruction
  TNtuple* _dimu_reco;

  //! nagle event ntuple...
  TNtuple* _cu_dimu_evt;

  //! one entry per reco track
  TNtuple* _single_reco;

  //! one entry per reco stub
  TNtuple* _stub_reco;

  //! one entry per reco clus
  TNtuple* _clus_reco;

  //! reconstruction efficiency
  TNtuple* _effic;

  //! background ntuple
  TNtuple* _background;

  //! muioo eval
  TNtuple* _muioo;

  //! bend plane eval
  TNtuple* _bend_plane;

  //! Colorado single muon ntuple
  TNtuple* _cu_mc_trk;

  //! Colorado single muon reconstructed track ntuple
  TNtuple* _cu_reco_trk;

  //! Colorado single muon reconstructed track ntuple with more information
  TNtuple* _cu_effic_nt;

  //! global (i.e. per event) evaluation
  PHGlobal* _global;

  // Event Selector
  //
  int _event_veto;

  #ifndef __CINT__

  //! number of events (internal counter)
  unsigned int _n_evt;

  //! number of roads of given depth
  boost::array<unsigned int,MUIOO::MAX_PLANE> _n_road_pl;

  //! max numbers of timers
  static const UShort_t n_timers = 30;

  //! stores timers information. Index corresponds to booking order.
  boost::array<double,n_timers>_time;

#endif

};

#endif /* __MUONFVTXEVAL_H__ */

