#ifndef __ACCCLUSTERV1_H
#define __ACCCLUSTERV1_H

#include "AccCluster.h"
#include <iostream>

//
//  This class is a so-called "versioned" object.  We are
//  inheriting from the AccCluster.  That virtual base 
//  class has manipulators for AccCluster objects that are
//  not specific to any particular verion of AccCluster objects.
//  This container over-rides the methods of the virtual base class
//  making them specific to version 1 AccClusters.
//
//  *Here* is where we actually make the TClonesArray that holds the
//  single AccClusterv1 objects.
//                                 TKH 6-4-2004
//

class AccSnglCluster;
class TClonesArray;

class AccClusterv1 : public AccCluster
{
 public:
  AccClusterv1();
  AccClusterv1(const AccClusterv1&);
  AccClusterv1& operator=(const AccClusterv1&);
  virtual ~AccClusterv1();

  AccClusterv1* clone() const;

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
  AccSnglCluster* AddCluster (const unsigned int itrk, const AccSnglCluster& sngl);
  AccSnglCluster* get_cluster(const unsigned int itrk) const;

 protected:
  TClonesArray *GetCluster() const {return Cluster;}
  unsigned int nCluster;
  TClonesArray *Cluster;

private:
  // Copy this to dest.
  void copyto(AccClusterv1& dest) const;
  ClassDef(AccClusterv1,1)
};

#endif /* __ACCCLUSTERV1_H */






