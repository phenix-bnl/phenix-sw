// ============================
// FILE: SvxGhitClusterListv1.h
// ============================
// ****************************************************************************
// Container for the SvxGhitClusterv1
// This contains all SvxGhitCluster objects in an event.
// ---
// Created  by V. L. Rykov on 15-Feb-2004
//
// Modified by V. L. Rykov on 11-May-2004:
//     Sorting flags & methods and index search added.
// ****************************************************************************

#ifndef __SVXGHITCLUSTERLISTV1_HH_
#define __SVXGHITCLUSTERLISTV1_HH_

#include "TClonesArray.h"
#include "SvxGhitClusterList.h"
#include "SvxGhitClusterv1.h"

class SvxGhitClusterListv1 : public SvxGhitClusterList
{
 private:
  enum {SVXNGHITCLUSTER = 20000};   //! Default initial container size

 protected:
  // Data
  unsigned int  nGhitClusters   ; //  Number of hits in the container
  TClonesArray* GhitCluster     ; //  Hit container
  bool          ghitSorted      ; //! Sorting flag for ghits
  bool          clusterSorted   ; //! Sorting flag for clusters
  bool          listPointersSet ; //! Each SvxGhitClusterv1 knows list pointer
  // Methods
  void SortSTL();
  void restoreListPointers(); // Give list pointer to each SvxGhitClusterv1

 public:
  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxGhitClusterListv1(const unsigned int length = SVXNGHITCLUSTER);
  virtual ~SvxGhitClusterListv1();

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset    ();
  int  isValid  () const;
  void identify (std::ostream &os=std::cout) const;

  // Add/remove/set/get methods...
  // """""""""""""""""""""""""""""
  SvxGhitCluster* addGhitCluster    (const          int ihit = -1);
  void            removeGhitCluster (const unsigned int ihit     );

  SvxGhitCluster* get_GhitCluster   (const unsigned int ihit) const
    { return (SvxGhitCluster*) GhitCluster->UncheckedAt(ihit) ;}
  int             get_nGhitClusters ()                        const
    { return (int) nGhitClusters                              ;}
  bool             check_ghitSorted ()                        const
    { return ghitSorted                                       ;}
  bool          check_clusterSorted ()                        const
    { return clusterSorted                                    ;}
  // Routines to manipulate the cluster array...
  // """""""""""""""""""""""""""""""""""""""""""
  int Compress();
  int set_TClonesArraySize(const unsigned int nhit);

  // Sorting and index search
  // """"""""""""""""""""""""
  void sortGhits   () ;
  void sortClusters() ;
  void unSort      () ;
  // Find first occurrence of ghit
//  int  indexOfGhit    (const int id        ,
//		       const int ifrom =  0,
//		       const int iupto = -1) const;
  // Find first occurrence of cluster
//  int  indexOfCluster (const int id        ,
//		       const int ifrom =  0,
//		       const int iupto = -1) const;
  bool indexOfGhit(const int id, int& idx_lb, int& idx_ub, 
                     int ifrom=0, int iupto=-1) const;
  bool indexOfCluster(const int id, int& idx_lb, int& idx_ub, 
                     int ifrom=0, int iupto=-1) const;
  
  // Methods
  // """""""
  void print() const;

  // ---
  ClassDef(SvxGhitClusterListv1,1)
};
#endif /* __SVXGHITCLUSTERLISTV1_HH_ */
