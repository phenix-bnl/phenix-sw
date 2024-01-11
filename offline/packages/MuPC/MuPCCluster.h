#ifndef __MUPCCLUSTER_HH_
#define __MUPCCLUSTER_HH_

#include "PHObject.h"
#include <iostream>

class MuPCSnglCluster;

class MuPCCluster : public PHObject
{


 public:
  virtual ~MuPCCluster() {}


  //  Virtual methods should be over-ridden...
  virtual void set_ncluster(const unsigned int NTRACK); 
  virtual int  get_ncluster() const ;

  //  "Set" functions add(remove) MuPCSnglCluster objects to(from) the collection...
  virtual int  set_TClonesArraySize(const unsigned int ntrk) {return 0;}
  virtual void AddCluster       (const unsigned int itrk) {return;}
  virtual void RemoveCluster    (const unsigned int itrk) {return;}
  virtual MuPCSnglCluster* AddCluster(const unsigned int itrk, const MuPCSnglCluster &cluster) {return NULL;}

  //  "Get" function retreives a pointer to any of the objects in the collection...
  virtual MuPCSnglCluster* get_cluster(const unsigned int itrk) const;

  //  "Clone" method allows to make additional containers based upon this one...
  virtual MuPCCluster* clone() const;


  // Standard functions of all inheritors of PHObject classes...
  virtual void Reset();
  virtual int isValid() const;
  virtual void identify(std::ostream &os = std::cout) const;


  ClassDef(MuPCCluster,1)
};
#endif /* __MUPCCLUSTER_HH_ */
