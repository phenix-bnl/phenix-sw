// $Id: FvtxConeTracklets.h,v 1.7 2013/02/24 21:41:17 jinhuang Exp $

#ifndef __FvtxConeTracklets_h__
#define __FvtxConeTracklets_h__

/*!
 \file		FvtxConeTracklets.cxx
 \ingroup supermodules
 \brief Make coordinate pairs projecting to the vertex and count them
 around each muon track
 \author	Cesar Luiz da Silva, Jin Huang
 \version $Revision: 1.7 $
 \date		$Date: 2013/02/24 21:41:17 $
 */

#include <MuonSubsysReco.h>

#include <string>
#include <vector>
#include <utility>
#include <PHGeometry.h>

#ifndef __CINT__

#include <TFvtxCoordMap.h>
#include <TFvtxTrkMap.h>
#include <TFvtxSvxClusterMap.h>
#include <TFvtxMPTrack.h>

#else

class TFvtxCoordMap;
class TFvtxTrkMap;
class TFvtxSvxClusterMap;
class TFvtxMPTrack;

#endif

// Forward declerations
class PHCompositeNode;
class PHMuoTrack;
class VtxOut;
class BbcOut;
class TNtuple;
class PHMuoTracksOut;
class TFvtxCompactTrkMap;

/*!
 \ingroup supermodules
 Fvtx Cluster pairs in the cone counter
 */
class FvtxConeTracklets : public SubsysReco
{

  //! @name Interface Calls
  //@{

public:

  //! constructor
  FvtxConeTracklets(std::string eval_name = "fvtxconetracklets.root");

  //! destructor
  virtual
  ~FvtxConeTracklets();

  //! module initialization
  int
  Init(PHCompositeNode *topNode);

  //! run initialization
  int
  InitRun(PHCompositeNode *topNode);

  //! event processing
  int
  process_event(PHCompositeNode *topNode);
//  int process_event_old(PHCompositeNode *topNode);

//! end of process
  int
  End(PHCompositeNode *topNode);

protected:

  VtxOut* vtx;
  PHMuoTracksOut* _muo;
  TFvtxCoordMap* _coord_map;
  TFvtxSvxClusterMap* _svx_map;
  TFvtxCompactTrkMap* _ctrk_map;
  TFvtxTrkMap * _trk_map;

  //@}

  //! @name Interface flags and parameters
  //@{

public:

  //! Obsolete
  //! Use BBC/SIM vertex if false/true
  void
  set_is_sim(bool a);

  //! use evaluation
  void
  set_make_eval(bool a)
  {
    make_eval = a;
  }

  //! max cone size
  void
  set_max_cone(float c)
  {
    _max_cone = c;
  }

  //! max cone size
  float
  get_max_cone() const
  {
    return _max_cone;
  }

  //! min cone size
  void
  set_min_cone(float c)
  {
    _min_cone = c;
  }

  //! min cone size
  float
  get_min_cone() const
  {
    return _min_cone;
  }

  //! dR -> bin number for logrithmic cone size
  size_t
  get_bin(double delta);

  //! min cut for logrithmic bin
  double
  get_bin_floor(size_t bin);

  //! max cut for logrithmic bin
  double
  get_bin_ceiling(size_t bin)
  {
    return get_bin_floor(bin + 1);
  }

  //! whether to save tracklet to TFvtxCompactTrkMap
  void
  set_save_tracklet(bool c)
  {
    _save_tracklet = c;
  }

  //! whether to save tracklet to TFvtxCompactTrkMap
  bool
  get_save_tracklet() const
  {
    return _save_tracklet;
  }

  //! which vertex to use.
  //! default is AUTO, which choose best of FVTX, FVTX_SECOND and BBC
  std::string
  get_vertex_name()
  {
    return _vertex_name;
  }

  //! which vertex to use.
  //! default is AUTO, which choose best of FVTX, FVTX_SECOND and BBC
  void
  set_vertex_name(std::string vertex_name)
  {
    _vertex_name = vertex_name;
  }

  //! only process event which have at least one MuTr track out
  bool
  get_muon_event_only()
  {
    return _muon_event_only;
  }

  //! only process event which have at least one MuTr track out
  void
  set_muon_event_only(bool a)
  {
    _muon_event_only = a;
  }

  //! \brief local copy of new nDST object
  //! if and only if v14 or older version of nDST is used in the top node,
  //! then the cone observables can be accessed through this pointer
  const PHMuoTracksOut *
  get_muo_local() const
  {
    return _muo_local;
  }

protected:

  //! whether to save tracklet to TFvtxCompactTrkMap
  bool _save_tracklet;

  //! max cone size
  float _max_cone;

  //! min cone size
  float _min_cone;

  enum
  {
    //! number of bins for track cone
    n_bin = 8
  };

  //! cut on phi acceptance window
  double phi_acpt_width_cut;

  //! cut on tracklet projection
  double w_acpt_cut;

  //! vertex cut
  double vertex_cut_min;

  //! get vertex cut as max of vertex_cut_min and 2 sigma
  double _vertex_cut;

  //! which vertex to use.
  //! default is AUTO, which choose best of FVTX, FVTX_SECOND and BBC
  std::string _vertex_name;

  //! vertex point used
  PHPoint _vertex;

  //! number of bad vertex
  int _n_bad_vertex;

  //! only process event which have at least one MuTr track out
  bool _muon_event_only;

  //! using V15 or newer nDST
  bool _is_new_nDST;

  //! local copy of new nDST object if older version is used in the DST node
  PHMuoTracksOut * _muo_local;

  //@}

  //! @name Tracking
  //@{

protected:

  //! Choose vertex to use
  int
  process_vertex();

  //! TFvtxTrkMap  -> tracklets, as well as replacing TFvtxCompactTrkMap if _save_tracklet
  void
  import_fvtx_trk();

  //! TFvtxSvxClusterMap  -> layer_list for vtx book keeping
  void
  import_vtx_clusters();

  //! status bit to mark whether a FVTX cluster has been used
  enum
  {
    fvtx_cluster_status_bit = (1 << 15)
  };

  //! Tracking starting from FVTX clusters: TFvtxCoordMap  -> tracklets, as well as TFvtxCompactTrkMap if _save_tracklet
  void
  make_tracklets_from_fvtx();

  //! Tracking starting from VTX clusters: TFvtxCoordMap & TFvtxSvxClusterMap -> tracklets, as well as TFvtxCompactTrkMap if _save_tracklet
  void
  make_tracklets_from_vtx();

  //! add more FVTX clusters to _trk from a seed station 1 and wedge 1
  void
  add_fvtx_clusters(const int iarm, const int istation1, const int wedge1);

  TFvtxMPTrack _trk;

  //@}

  //! @name Tracking service - Process clusters, events, evaluations
  //@{

protected:

#ifndef __CINT__

  typedef std::pair<int, double> fvtx_phi_cent_record;
  typedef std::vector<fvtx_phi_cent_record> fvtx_phi_cent_list_arm;
  std::vector<fvtx_phi_cent_list_arm> _fvtx_phi_cent_list;

  //! Add one FVTX cluster to tracklet and check consistency
  bool
  try_cluster(TFvtxCoordMap::pointer &coord);

  //! Add FVTX cluster to tracklet. service function for make_tracklets
  TFvtxMPNode *
  add_cluster(TFvtxCoordMap::pointer &coord);

  //! Add VTX cluster to tracklet. service function for make_tracklets
  TFvtxMPNode *
  add_cluster(const SvxCluster * clus);

#endif

  //! TFvtxMPTrack trk -> tracklets
  void
  store_tracklet();

  //! tracklets -> PHMuoTracksOut
  void
  save_tracklets();

  //! TFvtxCoordMap & TFvtxSvxClusterMap -> PHMuoTracksOut
  void
  process_clusters();

  int _eventCount;

  bool make_eval;
  std::string eval_name;
  TNtuple * ntup;

#ifndef __CINT__

  //! most compact descript of tracklet, pair of eta and phi
  typedef std::pair<double, double> tracklet_t;

  //! storage for tracklet
  std::vector<tracklet_t> _tracklets;

  enum
  {
    //!// only consider the first two layers
    n_vtx_layers = 2
  };

  //! record for local book keeping for vtx clusters, one for each cluster
  typedef std::pair<bool, const SvxCluster *> vtx_cluster_record;

  //! list for local book keeping for vtx clusters, one for each layer of VTX
  typedef std::vector<vtx_cluster_record> vtx_cluster_list;

  //! local book keeping for VTX clusters
  std::vector<vtx_cluster_list> _vtx_layer_list;

#endif

  //@}

};

#endif 
