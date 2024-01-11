// $Id: FvtxGlobalAlign.h,v 1.21 2015/09/09 01:50:47 jinhuang Exp $

#ifndef __FvtxGlobalAlign_h__
#define __FvtxGlobalAlign_h__

/*!
 \file  FvtxGlobalAlign.h
 \brief   fvtx global alignment module
 \author  Zhengyun You
 \version $Revision: 1.21 $
 \date  $Date: 2015/09/09 01:50:47 $
 */

#include "MuonSubsysReco.h"

#include <MUTOO.h>
#include <FVTXOO.h>
#include <FVTXGEOM.h>

#include <string>
#include <cstdarg>
#include <cstdio>
#include <utility>
#include <iostream>
#include <cassert>

#ifndef __CINT__
#include <boost/array.hpp>
#include <TFvtxTrkMap.h>
#include <TFvtxCoordMap.h>
#include <TFvtxSvxClusterMap.h>
#include <TMutTrkMap.h>
#include <TMutCoordMap.h>
#include <TFvtxClusMap.h>
#include <TMuiRoadMapO.h>
#include <TMuiClusterMapO.h>

#include <PHTrackIntegratorKF.h>
#include <PHTimeServer.h>
#include <PHPoint.h>
#include <EventHeader.h>
#include <RunHeader.h>
#else
class TFvtxTrkMap;
class TFvtxClusMap;
class TFvtxSvxClusterMap;
class TMutTrkMap;
class TMutCoordMap;
//class TFvtxAlignParMap;
class EventHeader;
class RunHeader;
#endif

#include <TMutTrkPar.hh>

#include <TFvtxMILLEPEDE.h>

// forward declaration for FVTXOO classes.
class TFvtxCoordMap;
class VtxOut;
class TTree;
class TFvtxMPTrack;
class TFvtxMPNode;
class TH1;

/*!
 \class   FvtxGlobalAlign
 \brief   fvtx global alignment tree generation.
 */
class FvtxGlobalAlign : public MuonSubsysReco, public TFvtxMILLEPEDE
{

  // --------------------------------------------------------------------------
  //!@name constructor, destructor and service functions
  //@{

public:

  //! constructor
  FvtxGlobalAlign(const char* name = "FVTXGLOBALALIGN");

  //! destructor
  virtual
  ~FvtxGlobalAlign();

  //! return module name
  virtual const char *
  Name() const
  {
    return ThisName.c_str();
  }

  //@}

  // --------------------------------------------------------------------------
  //!@name Data flow control
//@{

  //  void minimizer_tests(const int n_tracks = 10000);

  //! initializationm, call at the begining of event track loop
  void
  init(void);

  //! end filling tracks and do alignment, call at the end of event track loop
  void
  end(void);

  // ! main function for Fun4All event loops
  // ! one call per event to process tracks and do alignment (if DO_ALIGNMENT flag is set)
  void
  event(void);

  // ! main function for reading in alignment DSTs and do alignment
  void
  minimize_alignment_DST(TTree * AlignDST_tree, int nEvent = -1);

  //@}

  // --------------------------------------------------------------------------
  //!@name SubsysReco IO
  //@{

public:

  //! run initialization
  int
  Init(PHCompositeNode *top_node);

  //! event method
  int
  process_event(PHCompositeNode *top_node);

  //! finish run
  int
  End(PHCompositeNode *top_node);

protected:

  //! create all fvtxoo nodes
  int
  createNodeTree(PHCompositeNode *top_node);

  //! retrieve pointers to the needed maps
  void
  set_interface_pointers(PHCompositeNode *top_node);

  //! local pointer to the track map
  TFvtxTrkMap* _trk_map;

  //! local pointer to the cluster map
  TFvtxClusMap* _cluster_map;

  //! local pointer to the coord map
  TFvtxCoordMap* _coord_map;

  //! local pointer to the MuTr track map
  TMutTrkMap* _trk_mutr_map;

  //! local pointer to the MuTr coord map
  TMutCoordMap* _coord_mutr_map;

  // vertex node
  VtxOut* _vtxout_node;
  //@}

  // --------------------------------------------------------------------------
//!@name output results
//@{

public:

  //! changes alignment tree filename
  void
  set_misalignment_filename(const char* file)
  {
    if (file)
      _misalignment_filename = file;
  }

  //! changes dump filename (for millepede log)
  void
  set_dump_filename(const char* file)
  {
    if (file)
      _dumpfile_name = file;
  }

  //! export misaaglinemnt and dump all alignment parameters to filename
  bool
  export_misalignment(const char* filename);

  void
  set_AlignDST_filename(const char* filename)
  {
    _AlignDST_filename = filename;
  }

protected:
  //! evaluation method
  void
  initialize_AlignDST_tree(bool read = false);

  //! evaluation method
  void
  initialize_alignment_tree();

private:
  //! output TFile name for alignment tree
  std::string _misalignment_filename;

  //! Set an output file for the Dumpfile output
  const char* _dumpfile_name;

  //! output TFile name for evaluation tree
  std::string _AlignDST_filename;

  //! alignment tree (stores alignment evaluation informations track by track)
  TTree* _AlignDST_tree;

  //! alignment tree (stores alignment evaluation informations track by track)
  TTree* _AlignDST_tree_read;

  //! average beam position stuff
  TH1 * _h_beam_x;
  TH1 * _h_beam_y;

  //! Process - we can choose to output one PHMAP node from DST to Align DST
  void
  fill_container();

  //! Data member - we can choose to output one PHMAP node from DST to Align DST
  TClonesArray * _container;

#ifndef __CINT__
  //! module timer
  PHTimeServer::timer _timer;
  //@}
#endif

  // --------------------------------------------------------------------------
  //!@name flags
//@{

public:

// ! additional flag beyond TFvtxMILLEPEDE::Flag
  enum ExtraFlag
  {

    //! use cut for track selection for DST and millepede?
    USE_CUTS = 1 << 14,

    //! whether to do millepede alignment
    DO_ALIGNMENT = 1 << 15,

    //! whether to save alignment DST
    DO_ALIGN_DST = 1 << 16,

    //! obsolete
    DO_EVALUATION = DO_ALIGN_DST,

    //! obsolete - store alignment parameters but do not perform minimization
    WRITE_MEM = 1 << 17,

    //! obsolete - read alignment parameters from node and not from reconstructed maps
//    READ_MEM = 1 << 18,

    //! Scale MuTr kalman fit total momentum to be consistent with the joint fit
    SCALE_MUTR_KALMAN_MOMENTUM = 1 << 18,

    //! Use Millepede to produce initial track fitting (straight track fit)
    //! Otherwise, the default FVTX track fit (TFvtxTrkMap) will be used as the initial track fit (TFvtxMPTrack::use_kalman_fit -> 1)
    USE_MILLEPEDE_TRACK_FIT = 1 << 19,
    NOT_USE_KALMFAN_FIT = USE_MILLEPEDE_TRACK_FIT,

    //! historical name; fit track in 3-D space, but constraint it along central line of phi acceptance window
    TRACK_VZCON_Fit = 1 << 20,

    //! fit track in 3-D space, but constraint it along central line of phi acceptance window
    TRACK_LATCON_FIT = TRACK_VZCON_Fit,

    //! accept track with two hits on same station, one on each side
    DOUBLE_HIT_PER_STA = 1 << 21,

    //! only accept track if it can be originted from beam by cutting on valid phi acceptance window
    USE_PHI_ACPT_CUT = 1 << 22,

    //! use VTX vertex to constraint tracks on the pT direction
    USE_SVTX_CONSTRAINT = 1 << 23,

    //! constraint stations
    USE_CONSTRAINTS_STATIONS = 1 << 24,

    //! consider tracks without association with VTX or MuTr hits?
    USE_FVTX_ALONE_TRACK = 1 << 25,

    //! include VTX hits in the fitting ?
    USE_VTX_HITS = 1 << 26,

    //! include MuTr hits in the fitting ?
    USE_MUTR_HITS = 1 << 27,

    //! redo constraint calculation during minimize_alignment_DST
    RECAL_CONSTRAINT = 1 << 28,

    //! consider tracks without association with VTX or MuTr hits?
    USE_MUTR_ALONE_TRACK = 1 << 29,

    //! include MuID hits in the fitting ?
    USE_MUID_HITS = 1 << 30,

    //! obsolete
//    USE_THETA_CUT = 1 << 30,

    //! obsolete
    USE_THETA_CUT_CLOSE_TRACK = 1 << 31,

    NONE2 = 0
  };

  virtual void
  set_flag(const enum ExtraFlag& flag, const bool& value)
  {
    if (value)
      {
        _flags |= flag;
      }
    else
      {
        _flags &= (~flag);
      }
  }

  virtual bool
  get_flag(const enum ExtraFlag& flag) const
  {
    return _flags & flag;
  }

  virtual void
  set_flag(const Flag& flag, const bool& value)
  {
    TFvtxMILLEPEDE::set_flag(flag, value);
  }

  virtual bool
  get_flag(const Flag& flag) const
  {
    return TFvtxMILLEPEDE::get_flag(flag);
  }

  virtual void
  set_flag(const TFvtxMILLEPEDE::Flag_MuArm& flag, const bool& value)
  {
    TFvtxMILLEPEDE::set_flag(flag, value);
  }

  virtual bool
  get_flag(const TFvtxMILLEPEDE::Flag_MuArm& flag) const
  {
    return TFvtxMILLEPEDE::get_flag(flag);
  }

  //! alignment flag
  void
  set_do_alignment(bool value)
  {
    set_flag(DO_ALIGNMENT, value);
  }

  //! evaluation flag
  void
  set_do_evaluation(bool value)
  {
    set_flag(DO_ALIGN_DST, value);
  }

  //! theta cut flag
  void
  set_theta_cut(bool value)
  {
    std::cout << "FvtxGlobalAlign::set_theta_cut is obsolete" << std::endl;
    assert(0);
  }

  //! theta cut flag
  void
  set_theta_cut_keeping_closeTracks(bool value)
  {
    set_flag(USE_THETA_CUT_CLOSE_TRACK, value);
  }

  //@}

  // --------------------------------------------------------------------------
  //!@name beam position inputs
//@{

public:

  void
  set_vtx_vertex_name(std::string s)
  {
    _vtx_vertex_name = s;
  }

  const std::string &
  get_vtx_vertex_name() const
  {
    return _vtx_vertex_name;
  }

  //! call this function to load beam position data
  //! in the order of file >> run >> x >> y >> ex >> ey;
  void
  load_beam_xy_data(std::string data_file = "BeamPos.dat")
  {
    _beam_pos_xy.load_data(data_file, Verbosity());
  }

  //! get precise beam X-Y position from off-line fit of VTX vertex
  class BeamPosXY
  {
  public:
    //! load data files
    void
    load_data(std::string data_file = "BeamPos.dat", int verbose = 0);

    //! have this run in data file?
    bool
    have_run(int run);

    //! get beam position in x
    double
    get_x(int run);

    //! get beam position in y
    double
    get_y(int run);

  protected:
    //! position in x y
    typedef std::pair<double, double> pos;
    typedef std::map<int, pos> record;

    //! map to run -> pos
    record position;
    //! map to run -> weight = 1/error^2
    record weight;

  };

private:

  BeamPosXY _beam_pos_xy;

  std::string _vtx_vertex_name;

  //@}

  // --------------------------------------------------------------------------
  //!@name process events and event properties
//@{
public:

  // Unified event information processor, should call before process_trk and minimize_magnets_off
  bool
  process_evt();

  //! make the cuts to select the appropriated event
  bool
  accept_evt();

  //! Print muon arm data and return number of good muons
  int
  inspect_mutr();

  //! whether to reject the event for external DST output in Run4All cycle
  int
  evaluate_event();

private:

  //! header pointers
  RunHeader* _run_header;
  EventHeader* _event_header;

  // result for inspect_mutr(), num of good muon tracks
  int _n_good_muon_trk;

  //! to know if the event have any good track
  int _n_good_trk;

  //! track IDs
  int _run_num;
  int _event_num;

  // vertex infos from BBC and VTX

  double _vtxx; //! final vertex used
  double _vtxy; //! final vertex used
  double _vtxz; //! final vertex used
  double _vtxxp; //! vtx preceise vertex
  double _vtxyp; //! vtx preceise vertex
  double _vtxzp; //! vtx preceise vertex
  double _bbcz; //! bbc z vertex

  PHPoint _vtx_point; // precise vertex from vtx (if available) or unprecise vtx vertex (otherwise)

  //! to know if the event overall is accepted or not
  int _accept_evt;

  //@}

  // --------------------------------------------------------------------------
  //!@name process track
//@{

protected:

#ifndef __CINT__

  //! Unified track information processor including filling evaluation tree
  bool
  process_trk(TFvtxTrkMap::pointer);

  bool
  process_trk(TMutTrkMap::pointer);

  //! add a TFvtxCoord to internal track object (TFvtxMPTrack * _track)
  void
  process_fvtx_coord(TFvtxTrkMap::pointer, TFvtxCoordMap::const_pointer);

  //! add a TMutCoord to internal track object (TFvtxMPTrack * _track)
  void
  process_mutr_coord( //
      TFvtxTrkMap::pointer trk_ptr, //
      TMutTrkMap::const_pointer trk_mutr_ptr, //
      TMutCoordMap::pointer coord_ptr //
      );

  //! add a TFvtxSvxCluster to internal track object (TFvtxMPTrack * _track)
  //! \param switch_r_phi apply track constraint on the direction of R or Phi. true = R
  void
  process_vtx_cluster( //
      TFvtxTrkMap::pointer trk_ptr, //
      TFvtxSvxClusterMap::const_pointer trk_svx_ptr, //
      bool switch_r_phi //
      );

  //! Get best MuID road and fill with MuID hits if needed.
  void
  process_muid_road(TMuiRoadMapO::const_key_iterator mui_iter,
      TMutTrkMap::const_pointer trk_ptr);

  //! add a TMuiClusterO to internal track object (TFvtxMPTrack * _track)
  void
  process_muid_clus( //
      TFvtxTrkMap::pointer trk_ptr, //
      TMutTrkMap::const_pointer trk_mutr_ptr, //
      TMuiClusterMapO::pointer coord_ptr //
      );

  //! Call this after filling clusters, process constraint, etc.
  void
  process_after_filling();

  //! Fill vertex DCA from MuTr track
  void
  fill_mutr_disp_vtx();

  //! apply constraints to track fittings, assume all hit nodes have already been filled
  void
  process_trk_constraint();

  //! add FVTX - MuTr alignment constraints to track fittings,
  void
  process_fvtx_mutr_constraint();

#endif

  //! make the cuts to select the appropriated tracks
  bool
  accept_trk();

  //! make the cuts to select the appropriated tracks
  bool
  accept_trk_mutr();

protected:

  //! total number of recorded tracks
  int _n_tracks_total;

  //! total number of tracks analyzed
  int _n_tracks_analyzed;

  //! total number of recorded tracks
  int _n_mutr_tracks_total;

  //! total number of tracks analyzed
  int _n_mutr_tracks_analyzed;

  //! total number of tracks used for millepede
  int _n_tracks_minimized;

  //! total number of recorded events
  int _n_events_total;

  //! total number of recorded events
  int _n_events_acpt;

  //! total number of recorded events
  int _n_events_analyzed;

  //! id of track inside a event
  int _track_index;

  //! Track object for storing fit related info
  TFvtxMPTrack * _track;

  //! true of _track is a instance of TForwardMPTrack
  bool
  is_TForwardMPTrack();

  //! Class name for _track
  std::string _track_class;

  //! used to get closest Track parameter to a given z
  class closest_z_ftor
  {
  public:

    //! constructor
    closest_z_ftor(const double &z) :
        _z(z)
    {
    }

    //! predicate
    bool
    operator()(const TMutTrkPar& first, const TMutTrkPar& second)
    {
      return fabs(first.get_z() - _z) < fabs(second.get_z() - _z);
    }

  private:

    //! z coordinate to match
    double _z;

  };

#ifndef __CINT__

  //! get best kalman fit
  const TMutTrkPar *
  get_kalman_fit(TFvtxTrkMap::pointer trk_ptr);

  //! get kalman fit extraption (extrap_trk_par) at specific z (z_det)
  void
  get_kalman_fit(const double z_det, TMutTrkPar & extrap_trk_par);

  //! fill kalman fit to node, given initalized TrackIntegratorKF and node.p_det is filled
  void
  fill_node_kalman_fit(TFvtxMPNode * node);

protected:

  //! kalman integrator wrapper with debugging and error recovery
  class TrackIntegratorKF : protected PHTrackIntegratorKF
  {

  public:

    TrackIntegratorKF() :
        PHTrackIntegratorKF(), _trk_par_init(), _trk_par_extrap()
    {
    }

    //! Initialize integrator
    void
    initialize(const TMutTrkPar& trk_par)
    {
      PHTrackIntegratorKF::initialize(trk_par);

      _trk_par_init = trk_par;
      _trk_par_extrap = trk_par;
    }

    //! Extrapolate the system to specified z.
    void
    extrapolate(double z_det, bool start_from_init = false,
        bool verbose = false);

    //! Update TMutTrkPar with current integrator state
    void
    finish(TMutTrkPar& trk_par)
    {
      trk_par = _trk_par_extrap;
    }

  protected:

    TMutTrkPar _trk_par_init;
    TMutTrkPar _trk_par_extrap;

  };

  TrackIntegratorKF _integrator;

#endif
  //@}

  // --------------------------------------------------------------------------
  //!@name additional constrains
  //@{

public:

  //! vertex constraint on 3-D fits (TRACK_VZCON_Fit)
  double
  get_vertex_lateral_constraint()
  {
    return _vertex_lateral_constraint;
  }

  //! vertex constraint on 3-D fits (TRACK_VZCON_Fit)
  double
  set_vertex_lateral_constraint(const double c)
  {
    return _vertex_lateral_constraint = c;
  }

  //! vertex PT constraint on 3-D fits (USE_SVTX_CONSTRAINT)
  double
  get_vtx_xy_constraint()
  {
    return _vtx_dca_constraint;
  }

  //! vertex PT constraint on 3-D fits (USE_SVTX_CONSTRAINT)
  double
  set_vtx_xy_constraint(const double c)
  {
    return _vtx_dca_constraint = c;
  }

  //! Expected vertex PT shift on 3-D fits (USE_SVTX_CONSTRAINT)
  double
  get_vtx_dca_shift()
  {
    return _vtx_dca_shift;
  }

  //! Expected vertex PT shift on 3-D fits (USE_SVTX_CONSTRAINT)
  double
  set_vtx_dca_shift(const double c)
  {
    return _vtx_dca_shift = c;
  }

  //! vertex acceptance
  double
  get_vertex_acceptance()
  {
    return _vertex_acceptance;
  }

  //! vertex acceptance
  double
  set_vertex_acceptance(const double c)
  {
    return _vertex_acceptance = c;
  }

  //! vertex resolution requirement
  double
  get_vertex_z_resolution_cut()
  {
    return _vertex_z_resolution_cut;
  }

  //! vertex resolution requirement
  double
  set_vertex_z_resolution_cut(const double c)
  {
    return _vertex_z_resolution_cut = c;
  }

  //! sigma constraints on all SVX pixel hits
  double
  set_svx_hit_sigma_r(const double c)
  {
    return _svx_hit_sigma_r = c;
  }
  //! sigma constraints on all SVX pixel hits
  double
  set_svx_hit_sigma_phi(const double c)
  {
    return _svx_hit_sigma_phi = c;
  }

  //! multiplicable factor to FVTX sigma
  double
  get_fvtx_hit_sigma_factor()
  {
    return _fvtx_hit_sigma_factor;
  }

  //! multiplicable factor to FVTX sigma
  double
  set_fvtx_hit_sigma_factor(const double c)
  {
    return _fvtx_hit_sigma_factor = c;
  }

  //! minimal sigma resolution on all MuTr hits
  double
  get_mutr_hit_sigma_min()
  {
    return _mutr_hit_sigma_min;
  }

  //! minimal sigma resolution on all MuTr hits
  double
  set_mutr_hit_sigma_min(const double c)
  {
    return _mutr_hit_sigma_min = c;
  }

  //! additional sigma scale for MuID
  double
  get_muid_hist_sigma_scale_factor()
  {
    return _muid_hist_sigma_scale_factor;
  }

  //! additional sigma scale for MuID
  double
  set_muid_hist_sigma_scale_factor(const double c)
  {
    return _muid_hist_sigma_scale_factor = c;
  }

  //! minimal sigma resolution on all MuTr hits
  double
  get_z_ref()
  {
    return _z_ref;
  }

  //! minimal sigma resolution on all MuTr hits
  double
  set_z_ref(const double c)
  {
    return _z_ref = c;
  }

  //! mutr minimal abs(pz)
  double
  get_pz_min()
  {
    return _pz_min;
  }

  //! mutr minimal abs(pz)
  double
  set_pz_min(const double c)
  {
    return _pz_min = c;
  }

  //! mutr maximal abs(pz)
  double
  get_pz_max()
  {
    return _pz_max;
  }

  //! mutr maximal abs(pz)
  double
  set_pz_max(const double c)
  {
    return _pz_max = c;
  }

  double
  get_sigma_dr_lateral() const
  {
    return _sigma_dr_lateral;
  }

  void
  set_sigma_dr_lateral(double sigmaDrLateral)
  {
    _sigma_dr_lateral = sigmaDrLateral;
  }

  double
  get_sigma_dr_pt() const
  {
    return _sigma_dr_pt;
  }

  void
  set_sigma_dr_pt(double sigmaDrPt)
  {
    _sigma_dr_pt = sigmaDrPt;
  }

  double
  get_sigma_dtheta_lateral() const
  {
    return _sigma_dtheta_lateral;
  }

  void
  set_sigma_dtheta_lateral(double sigmaDthetaLateral)
  {
    _sigma_dtheta_lateral = sigmaDthetaLateral;
  }

  double
  get_sigma_dtheta_pt() const
  {
    return _sigma_dtheta_pt;
  }

  void
  set_sigma_dtheta_pt(double sigmaDthetaPt)
  {
    _sigma_dtheta_pt = sigmaDthetaPt;
  }

private:

  //! vertex constraint on 3-D fits (TRACK_VZCON_Fit)
  double _vertex_lateral_constraint;

  //! vertex acceptance
  double _vertex_acceptance;

  //! vertex resolution requirement
  double _vertex_z_resolution_cut;

  //! mutr minimal abs(pz)
  double _pz_min;

  //! mutr maximal abs(pz)
  double _pz_max;

  //! vertex PT constraint on 3-D fits (USE_SVTX_CONSTRAINT)
  double _vtx_dca_constraint;

  //! vertex PT shift on 3-D fits (USE_SVTX_CONSTRAINT)
  double _vtx_dca_shift;

  //! sigma constraints on all SVX pixel hits
  double _svx_hit_sigma_r;
  double _svx_hit_sigma_phi;

  //! multiplicable factor to FVTX sigma
  double _fvtx_hit_sigma_factor;

  //! minimal sigma resolution on all MuTr hits
  double _mutr_hit_sigma_min;

  //! additional sigma scale for MuID
  double _muid_hist_sigma_scale_factor;

  //! minimal sigma resolution on all MuTr hits
  double _z_ref;

  double _sigma_dr_pt;

  double _sigma_dr_lateral;

  double _sigma_dtheta_pt;

  double _sigma_dtheta_lateral;

  //@}

};

#endif

