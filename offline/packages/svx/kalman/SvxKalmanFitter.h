//-------------------------------------------------------------------
// SvxKalmanFitter.h
// Author: Matt Wysocki, July 1, 2011
//
/// Description:
/// This is the class that actually runs the Kalman filter method.  It
/// is a Fun4All module that reads SvxSegments and their corresponding
/// SvxClusters off the node tree and refits the cluster positions.
/// This could be modified in the future to not need pre-reconstructed
/// SvxSegments, but that is currently not the case.
///
/// The actual fitting is done by creating an SvxKalmanNode for each
/// cluster and filling in the required information.  Then the
/// Kalman-filtering methods predict() and filter() are called for
/// each node.
//-------------------------------------------------------------------

#ifndef __SVXKALMANFITTER_H__
#define __SVXKALMANFITTER_H__

#include <SubsysReco.h>
#include <PHTimeServer.h>
#include <McEvalSingleList.h>

#ifndef __CINT__
#include <svxDetectorGeo.hh>
#include "SvxKalmanNode.h"
#include "DCKalmanNode.h"
#include "RKTrackRep.h"
#endif // __CINT__

#include <map>
#include <vector>
#include <string>

#include <SvxTrackFit_v1.h>

class PHCompositeNode;
class VtxOut;
class SvxGhitList;
class SvxGhitRawhitList;
class SvxRawhitClusterList;
class SvxClusterList;
class SvxSegmentList;
class SvxCentralTrackList;
class SvxTrackList;
class SvxSegment;
class SvxCentralTrack;
class SvxTrack;
class TTree;
class TFile;
class TreeOut;
class PHCentralTrack;
class KFDebugTree;

class SvxKalmanFitter : public SubsysReco
{
 public:
  static SvxKalmanFitter* get();

  SvxKalmanFitter(bool use_real_field, const char* outfilename="");
  ~SvxKalmanFitter();

  void set_use_real_fieldmap(bool b) {use_real_fieldmap_=b;}  ///< use a realistic field map or a uniform field
  void set_field_filename(const char* s) {field_filename_=s;}
  void set_field_scale_factor(float f) {RKTrackRep::set_field_scale_factor(f);}
  void setup_magnetic_field();

  void set_step_size_limit(const double& limit);

  /// fit the track to a helix to initialize the parameters
  SvxTrackFit_v1 fast_fit_track(SvxTrack* track);

  /// fill the SvxTrack parameters from SvxCentralTrack and PHCentralTrack
  SvxTrackFit_v1 fill_dc_info(SvxTrack* track);

  void set_use_central_tracks(bool b) {reading_central_tracks_ = b;}

  void set_use_vertex(bool b) {use_vertex_ = b;}
  void set_reverse_direction(bool b) {reverse_direction_ = b;}

  int GetNodes(PHCompositeNode *topNode);

  int Init(PHCompositeNode *topNode) {return 0;}
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  
  void SetVerbosity(int v)  {Verbosity(v);}
  
 protected:
#ifndef __CINT__
  static SvxKalmanFitter* instance_;

  /// converts one SvxSegment to SvxTrack
  SvxTrack* create_track_from_segment(SvxSegment*);

  /// converts SvxCentralTrack to SvxTrack
  SvxTrack* create_track_from_centraltrack(SvxCentralTrack*);

  /// Kalman fit this SvxSegment
  bool fit_track(SvxSegment* segment);

  /// Kalman fit this SvxCentralTrack
  bool fit_track(SvxCentralTrack* central_track);

  /// Kalman fit this SvxTrack
  bool fit_track(SvxTrack* track_ptr, bool use_vertex=false, bool reverse_direction=false);

  svxDetectorGeo* geom_; ///< the current geometry, read from svxPisa.par

  std::vector<SvxKalmanNode*> svxnodes;  ///< store the Kalman node objects
  DCKalmanNode dcnode;

  ///@{
  ///@name Pointers to the necessary reconstruction objects:
  VtxOut* vtxout_;
  PHCentralTrack* cnt_;
  McEvalSingleList* mc_eval_list_;
  SvxGhitList* ghit_list_;
  SvxGhitRawhitList* ghit_rawhit_list_;
  SvxRawhitClusterList* rawhit_cluster_list_;
  SvxClusterList* cluster_list_;
  SvxSegmentList* segment_list_;
  SvxCentralTrackList* central_track_list_;
  SvxTrackList* track_list_;
  SvxSegment* current_segment_;
  SvxCentralTrack* current_central_track_;
  ///@}

  double default_step_size_limit_; ///< default_step_size_limit_ (from Geant4, not currently used)
  float _mag_field_center; ///< value of the magnetic field z-component at x=(0,0,0)
  float _cm_to_gev; ///< conversion from radius in cm to pT  in GeV/c (given the _mag_field_center)

  bool read_par_from_file_; ///< whether to read the geometry from a file or the database
  bool include_vtx_pos_;    ///< whether the measured collision vertex is given small uncertainties in fit
  bool use_vertex_;
  bool reverse_direction_;  ///< whether to start at the outermost layer and propagate inwards
  bool use_real_fieldmap_;  ///< use a realistic field map or a uniform field
  std::string field_filename_;
  bool reading_segments_;   ///< true if we are converting SvxSegment to SvxTrack
  bool reading_central_tracks_;   ///< true if we are converting SvxCentralTrack to SvxTrack

  PHTimeServer::timer _timer;   ///< per-event timer

  KFDebugTree* tree_; ///< the debugging TTree (if DEBUG is defined in preprocessor)
#endif // __CINT__
};

#endif //__SVXKALMANFITTER_H__
