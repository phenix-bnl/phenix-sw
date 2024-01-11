
#ifndef __SVXTRACK_H__
#define __SVXTRACK_H__

///===================================
///  \file SvxTrack.h
///  \brief SVX DST Track object 
///  \author Michael P. McCumber
///===================================

// standard includes
#include <vector>

// PHENIX includes
#include <PHObject.h>

// SVX includes
#include "SvxTrackFit.h"
#include "SvxCluster.h"

/// \class SvxTrack
///
/// \brief The SVX DST track object
///
/// This class provides the base class for
/// the versioned track objects.
///
class SvxTrack : public PHObject
{
 public:
  // unversioned constructor/destructors
  SvxTrack() {}                ///< default constructor
  virtual ~SvxTrack() {}       ///< default destructor

  virtual void print(std::ostream& os = std::cout, bool max=false) const {} ///< long form content print

  // fit access methods
  //virtual const fit_list_type* get_fit_list() const {return 0;}       ///< return the list of fits 
  virtual bool has_fit(SvxTrackFit::FitType type) const {return 0;} ///< check if a particular track fit exists
  virtual SvxTrackFit* get_fit(SvxTrackFit::FitType type) {return 0;} ///< get a particular track fit
  virtual const SvxTrackFit* get_fit(SvxTrackFit::FitType type) const {return 0;} ///< get a particular track fit

  virtual void add_fit(const SvxTrackFit*) {}                    ///< add a particular track fit
  virtual size_t get_n_fits() const {return 0;}                       ///< get number of stored track fits
  virtual void clear_fits() {}                                 ///< reset track fit storage
  
  // cluster access methods
  virtual size_t get_n_clusters() const {return 0;}                      ///< get the number of associated clusters
  virtual std::vector<SvxCluster*> get_clusters() const {return std::vector<SvxCluster*>();}
  virtual std::vector<SvxCluster*> get_clusters_ordered() const {return std::vector<SvxCluster*>();}
  virtual std::vector<SvxCluster*> get_clusters_in_layer(UShort_t ilayer) const
  {return std::vector<SvxCluster*>();}
  virtual bool has_cluster_in_layer(UShort_t ilayer) const {return false;}   ///< determine if an associated cluster exists in ilayer ([0,3])
  virtual bool has_cluster(const SvxCluster* clus_ptr) const {return false;} ///< determine if a cluster is associated with this track
  virtual bool has_cluster(const int cluster_id) const {return false;}      ///< determine if a cluster is associated with this track

  virtual void pushback_cluster_pointer(SvxCluster*) {} ///< add just a cluster pointer onto the list (this is for SvxTrackList::load_associations())
  virtual void add_cluster(SvxCluster *cluster) {}     ///< add an associated cluster
  virtual void remove_cluster(SvxCluster *cluster) {}  ///< remove a particular associated cluster
  virtual void clear_clusters() {}                     ///< reset the cluster associations
  virtual std::vector<int> get_cluster_id_list() const {return std::vector<int>();}

 private:
  ClassDef(SvxTrack,1)
};
  
#endif // __SVXTRACK_H__
