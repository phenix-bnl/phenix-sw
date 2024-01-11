#ifndef __SVXHOUGHTRANSFORM_H__
#define __SVXHOUGHTRANSFORM_H__
//===========================================================
/// \file SvxHoughTransform.h
/// \brief A fun4all implementation of Alan's Hough Transform
/// \author Michael P. McCumber and Theodore Koblesky
/// got to https://www.phenix.bnl.gov/WWW/offline/wikioffline/index.php/SvxHoughTransform
//===========================================================

// PHENIX includes
#include <SubsysReco.h>
#include <Fun4AllReturnCodes.h>
#include <PHTimeServer.h>

// SVX includes

// Helix Hough includes

#ifndef __CINT__
// hiding this stuff from the ROOT interpreter which hates templates
#include <SimpleHit3D.h>
#include <SimpleTrack3D.h>
#include <sPHENIXTracker.h>
#include <VertexFinder.h> 
#endif // __CINT__

// standard includes
#include <vector>

// forward declarations
class PHCompositeNode;

/// \class SvxHoughTransform
///
/// \brief A fun4all implementation of Alan's Hough Transform
///
/// This module run Alan's Hough Transform and quick vertex finding
/// on the SvxClusterList of the event. It will produce both
/// SvxTrack and SvxSegments for the time being.
///
class SvxHoughTransform : public SubsysReco
{

 public:
 
  SvxHoughTransform(const std::string &name = "SVXHOUGHTRANSFORM");
  virtual ~SvxHoughTransform() {}

  // standard PHENIX calls
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode) {return EVENT_OK;}
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  
  void setVerbosity(int verb) {
    _verbosity = verb;
    //if(_tracker_init) _tracker_init->setVerbosity(verb);
    //if(_tracker) _tracker->setVerbosity(verb);
  } ///< set verbosity

  void setChi2_cut(double chi2_cut) { _chi2_cut = chi2_cut;} ///< set the tracking chi2 cut
  double getChi2_cut() { return _chi2_cut;}                  ///< get the tracking chi2 cut

  void setKappa_cut(double kappacut) { _kappacut = kappacut; } ///< set the tracking curvature cut (cm^-1)
  double getKappa_cut() { return _kappacut;}                   ///< get the tracking curvature cut (cm^-1)

  void setRequired_hits(unsigned int required_hits) { _required_hits = required_hits;} ///< set the number of hits required to construct a track
  unsigned int getRequired_hits() {return _required_hits;}                             ///< get the number of hits required to construct a track

  void setXY_cut(double xycut) { std::cout<<"THIS FUNCTION HAS BEEN DEPRECATED"<<std::endl;} ///< set the distance of closest approact cut for adding hits
  double getXY_cut() { std::cout<<"THIS FUNCTION HAS BEEN DEPRECATED"<<std::endl;return 0;}  ///< set the distance of closest approact cut for adding hits

  void setTracks_Per_Hit(int tracks_per_hit) { std::cout<<"THIS FUNCTION HAS BEEN DEPRECATED"<<std::endl;} ///< set the number of times any given hit may be used
  int getTracks_Per_Hit(){std::cout<<"THIS FUNCTION HAS BEEN DEPRECATED"<<std::endl; return 0;}                             ///< get the number of times any given hit may be used

  void setZ_cut(double z_cut){std::cout<<"THIS FUNCTION HAS BEEN DEPRECATED"<<std::endl;} ///< set maximum difference which hits can deviate from helix; note 0 equals apply no z_cut
  double getZ_cut(){std::cout<<"THIS FUNCTION HAS BEEN DEPRECATED"<<std::endl; return 0;} ///< get z_cut

  void setMax_Count(int max_count){std::cout<<"THIS FUNCTION HAS BEEN DEPRECATED"<<std::endl;}
	
#ifndef __CINT__
  // hiding the HelixHough object templates (Eigen mostly) from the ROOT interpreter

 private:

  // detector description
  static const float _magField = 0.92;  ///< Tesla for PHENIX in ++ = 0.92;
  static const float _cmToGeV = 0.92/333.6;   ///< radius of curvature conversion (radius of curvature for a 1 GeV/c particle in 1 Tesla is 333.6 cm)
  int _nlayers;                  ///< number of detector layers
  std::vector<float> _radii;          ///< radial distance of each layer (cm)
  std::vector<float> _smear_xy_layer; ///< detector hit resolution in phi (cm)
  std::vector<float> _smear_z_layer;  ///< detector hit resolution in z (cm)

  // object storage
  std::vector<SimpleHit3D> _clusters; ///< working array of clusters
  std::vector<SimpleTrack3D> _tracks; ///< working array of tracks
  std::vector<float> _vertex;         ///< working array for collision vertex

  // track finding routines
  sPHENIXTracker *_tracker_init; ///< initial vertex determination (loose track finding settings)
  sPHENIXTracker *_tracker;      ///< finds tracks with the updated vertex (tighter track finding settings)

  // vertex determination routine
  VertexFinder _vertexFinder; ///< vertex finding object

  // timers
  PHTimeServer::timer _timer;
  PHTimeServer::timer _timer_initial_hough;
  PHTimeServer::timer _timer_full_hough;

  // internal settings
  int _verbosity; ///< verbosity

  unsigned int _min_hits;
  unsigned int _min_hits_init;
  unsigned int _max_hits_init;
  unsigned int _max_hits;
  unsigned int _maxtracks;

  //--- old possibly deprecated ---
	//I will leave some of these here just in case we want them later, the framework is here. 
	//I only removed variables which could have no use in the new HelixHough version. -Theo 1/10/2012

  unsigned int _required_hits; ///< minimum number of hits to make a track
  double _kappacut;            ///< storage cut for track curvature
  double _chi2_cut;            ///< fast matrix fit quality chisq/dof              
  float _phi_cut;              ///< cut on phi during addBestHit and in ZHough_findHelices

#endif // __CINT__

};

#endif // __SVXHOUGHTRANSFORM_H__
