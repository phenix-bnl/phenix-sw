#ifndef __MUPCCLUSTERV1_H
#define __MUPCCLUSTERV1_H

#include "MuPCCluster.h"
#include <iostream>

class MuPCSnglCluster;
class TClonesArray;

class MuPCClusterv1 : public MuPCCluster
{
 public:
  MuPCClusterv1();
  MuPCClusterv1(const MuPCClusterv1&);
  MuPCClusterv1& operator=(const MuPCClusterv1&);
  virtual ~MuPCClusterv1();

  MuPCClusterv1* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os = std::cout) const;

  // Actual implementations of the set/get methods...
  void set_ncluster      (const unsigned int NCLUSTER) {nCluster = NCLUSTER; return;}
  int  get_ncluster      () const {return nCluster;}

  // Routines to manipulate the particle array...
  int set_TClonesArraySize(const unsigned int ntrk);
  void AddCluster          (const unsigned int itrk);
  void RemoveCluster       (const unsigned int itrk);
  MuPCSnglCluster* AddCluster (const unsigned int itrk, const MuPCSnglCluster& sngl);
  MuPCSnglCluster* get_cluster(const unsigned int itrk) const;

 protected:
  TClonesArray *GetCluster() const {return Cluster;}
  unsigned int nCluster;
  TClonesArray *Cluster;

private:
  // Copy this to dest.
  void copyto(MuPCClusterv1& dest) const;
  ClassDef(MuPCClusterv1,1)
};

#endif /* __MUPCCLUSTERV1_H */






