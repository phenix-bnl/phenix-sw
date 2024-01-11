#ifndef __SVXMASTERRECO_H__
#define __SVXMASTERRECO_H__

// ======================================
/// \file SvxMasterReco.h
/// \brief Master VTX reconstruction module
/// \author Michael P. McCumber
// ======================================

// PHENIX includes
#include <SubsysReco.h>
#include <phool.h>

// standard includes
#include <vector>

// forward declarations
class PHCompositeNode;

/// \class SvxMasterReco
///
/// \brief Master VTX reconstruction module
///
/// This meta-module will collect the
/// various modules in the SVX library as well
/// as settings into a single place for better
/// record keeping and to insure uniform 
/// reconstruction macros amoung all users.
///
/// Usage in Fun4All macros:
///
/// se->registerSubsystem(new SvxMasterReco()); // for read data
/// se->SetSimulated(); // add for simulated data
///
class SvxMasterReco : public SubsysReco
{

 public:

  SvxMasterReco(const std::string &name = "SVXMASTERRECO"); ///< create object and fill submodule list
  virtual ~SvxMasterReco();

  int Init(PHCompositeNode *topNode);          ///< call Init on all modules
  int InitRun(PHCompositeNode *topNode);       ///< call InitRun on all modules
  int process_event(PHCompositeNode *topNode); ///< etc...
  int ResetEvent(PHCompositeNode *topNode);    ///< etc...
  int Reset(PHCompositeNode *topNode);         ///< etc...
  int EndRun(const int runnumber);             ///< etc...
  int End(PHCompositeNode *topNode);           ///< etc...

  void Print(const std::string&) const {}

  void Verbosity(int verbosity); ///< set the verbosity on all modules

  void SetClustering(bool doClustering) {_doClustering = doClustering;} ///< turn on/off the clustering
  void SetTracking(bool doTracking) {_doTracking = doTracking;}           ///< turn on/off the tracking

  void SetSimulated() {CreateModuleList(true);} ///< reconfigure for passing simulated data

  void SetSvxTrackInterface(bool runSvxTrack) {_runSvxTrack = runSvxTrack;} ///< swap between old and new reconstruction interface

 protected:

  void CreateModuleList(bool readSimulationData = false);

  std::vector<SubsysReco*> _modules; ///< list of modules to run

  bool _doClustering;                ///< optional clustering
  std::vector<bool> _hasClusterDep;  ///< list of modules that make or require clusters

  bool _doTracking;                ///< optional tracking
  std::vector<bool> _hasTrackDep;  ///< list of modules that make or require tracks
  
  bool _runSvxTrack; ///< run the SvxTrack DST interface modules
};

#endif // __SVXMASTERRECO_h__
