#ifndef __ACCCLUSTER_HH_
#define __ACCCLUSTER_HH_

#include "PHObject.h"
#include <iostream>

//
//  Hello all
//
//    This is the AccCluster class.  The Acc device is different than 
//  many others in PHENIX.  The primary reason for this difference is that
//  the ACC is sometimes a "hit" device (tracks should hit it) and sometimes
//  a veto device (track should not hit it).  For this reason, each track that
//  projects into the detector forms a region which could be considered as a 
//  "cluster" not based upon the ACC response, but solely based upon the track
//  location.
//
//    Nonetheless, we should strive to have an object that corresponds to the
//  "thing" that is associated with a particular track.  This "thing" has been 
//  referred to as a quad-box in some contexts but is called a Cluster here.
//  This class is the virtual base class that acts as a container for a set 
//  of individual (or Sngl) Clusters.  The user receives a pointer to the base
//  class of a single Cluster from here and then operates in that SnglCluster
//  to deduce the internal information.  All these things will be filled using the
//  AccclusterReco() object that inherits from the SubsysReco() base class.
//
//                                    TKH 6-4-2004
//

class AccSnglCluster;

class AccCluster : public PHObject
{


 public:
  virtual ~AccCluster() {}


  //  Virtual methods should be over-ridden...
  virtual void set_ncluster(const unsigned int NTRACK); 
  virtual int  get_ncluster() const ;

  //  "Set" functions add(remove) AccSnglCluster objects to(from) the collection...
  virtual int  set_TClonesArraySize (const unsigned int ntrk) {return 0;}
  virtual void AddCluster           (const unsigned int itrk) {return;}
  virtual void RemoveCluster        (const unsigned int itrk) {return;}
  virtual AccSnglCluster* AddCluster(const unsigned int itrk, const AccSnglCluster &cluster) {return NULL;}

  //  "Get" function retreives a pointer to any of the objects in the collection...
  virtual AccSnglCluster* get_cluster(const unsigned int itrk) const;

  //  "Clone" method allows to make additional containers based upon this one...
  virtual AccCluster* clone() const;

  // Standard functions of all inheritors of PHObject classes...
  virtual void Reset();
  virtual int isValid() const;
  virtual void identify(std::ostream &os = std::cout) const;

  ClassDef(AccCluster,1)
};
#endif /* __ACCCLUSTER_HH_ */
