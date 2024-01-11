#ifndef __SVXTRACK_V1_H__
#define __SVXTRACK_V1_H__

//=============================
/// \file SvxTrack_v1.h
/// \brief DST track object
/// \author Michael P. McCumber
//=============================

// Svx includes
#include "SvxTrack.h"
#include "SvxTrackFit_v1.h"
class SvxCluster;

#include <functional>
#include <algorithm>
#include <set>
#include <map>

/// \class SvxTrack_v1
///
/// \brief Versioned SVX DST track object
///
/// This class holds a set of track fits which
/// in turn hold a set of track projections.
///
class SvxTrack_v1 : public SvxTrack
{
 public:
  // convenience typedefs
  typedef std::map<SvxTrackFit::FitType,SvxTrackFit_v1> fit_list_type;
  typedef SvxTrackFit_v1* fit_ptr;
  typedef fit_list_type::iterator fit_iter;
  
  typedef std::vector<SvxCluster*> cluster_list_type;               ///< Name of the list of SvxCluster associations for this object
  typedef cluster_list_type::iterator cluster_iterator; ///< Name of a const iterator to residual list 
  typedef cluster_list_type::const_iterator const_cluster_iterator; ///< Name of a const iterator to residual list 

  // versioned constructors/destructors
  SvxTrack_v1();
  virtual ~SvxTrack_v1() {}

  void print(std::ostream& os = std::cout, bool max=false) const;  ///< long form content print

  // fit access methods
  //const fit_list_type* get_fit_list() const {return &_fit_list;}  ///< get the full list of track fits 
  bool has_fit(SvxTrackFit::FitType type) const;                              ///< check if a particular track fit exists
  SvxTrackFit* get_fit(SvxTrackFit::FitType type);                            ///< get a particular track fit
  const SvxTrackFit* get_fit(SvxTrackFit::FitType type) const;                ///< get a particular track fit (const version)
  size_t get_n_fits() const {return _fit_list.size();}            ///< get number of stored track fits

  void add_fit(const SvxTrackFit* fit);                           ///< add a particular track fit
  void clear_fits() {_fit_list.clear();}                          ///< reset track fit storage
  
  // cluster access methods
  bool has_cluster_in_layer(UShort_t ilayer) const;               ///< determine if an associated cluster exists in ilayer ([0,3])
  bool has_cluster(const SvxCluster* clus_ptr) const;              ///< determine if a cluster is associated with this track
  bool has_cluster(const int cluster_id) const;              ///< determine if a cluster is associated with this track
  std::vector<SvxCluster*> get_clusters() const;
  std::vector<SvxCluster*> get_clusters_ordered() const;
  std::vector<SvxCluster*> get_clusters_in_layer(UShort_t ilayer) const;

  void add_cluster(SvxCluster *cluster);                                    ///< add a cluster onto the list
  void pushback_cluster_pointer(SvxCluster *cluster);                       ///< add just a cluster pointer onto the list (this is for SvxTrackList::load_associations())
  void remove_cluster(SvxCluster *cluster);                                 ///< remove a cluster from the list
  size_t get_n_clusters() const {return _cluster_list.size();}              ///< get the number of associated clusters
  void clear_clusters() {_cluster_list.clear(); _cluster_id_list.clear();}   ///< reset the cluster associations

  std::vector<int> get_cluster_id_list() const {return _cluster_id_list;}

 protected:	
  fit_list_type _fit_list;           ///< list of unique fits to the track 
  std::vector<int> _cluster_id_list; ///< list of IDs of associated clusters
  cluster_list_type _cluster_list;   //!< list of pointers to associated clusters

  // comparison operator for SvxCluster based on radius (used in std::sort())
  struct cluster_radius_less_than : std::binary_function <SvxCluster*,SvxCluster*,bool> {
    bool operator() (const SvxCluster* c1, const SvxCluster* c2) const
    {
      float r1 = sqrt(c1->get_xyz_global(0)*c1->get_xyz_global(0) + c1->get_xyz_global(1)*c1->get_xyz_global(1));
      float r2 = sqrt(c2->get_xyz_global(0)*c2->get_xyz_global(0) + c2->get_xyz_global(1)*c2->get_xyz_global(1));
      return r1<r2;
    }
  };

 private:
  ClassDef(SvxTrack_v1,1)
};
  
#endif // __SVXTRACK_V1_H__
