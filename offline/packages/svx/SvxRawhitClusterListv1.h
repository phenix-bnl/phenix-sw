// ==============================
// FILE: SvxRawhitClusterListv1.h
// ==============================
// ****************************************************************************
// Container for the SvxRawhitClusterv1
// This contains all SvxRawhitCluster objects in an event.
// ---
// Created  by V. L. Rykov on 15-Feb-2004
//
// Modified by V. L. Rykov on 11-May-2004:
//     Sorting flags & methods and index search added.
// ****************************************************************************

#ifndef __SVXRAWHITCLUSTERLISTV1_HH_
#define __SVXRAWHITCLUSTERLISTV1_HH_

#include "TClonesArray.h"
#include "SvxRawhitClusterList.h"
#include "SvxRawhitClusterv1.h"

class SvxRawhitClusterListv1 : public SvxRawhitClusterList
{
 private:
  enum {SVXNRAWHITCLUSTER = 20000};   //! Default initial container size

 protected:
  // Data
  unsigned int  nRawhitClusters ; //  Number of hits in the container
  TClonesArray* RawhitCluster   ; //  Hit container
  bool          rawhitSorted    ; //! Sorting flag for rawhits
  bool          clusterSorted   ; //! Sorting flag for clusters
  bool          listPointersSet ; //! Each SvxRawhitClusterv1 has list pointer
  // Methods
  void SortSTL();
  void restoreListPointers(); // Give list pointer to each SvxRawhitClusterv1

 public:
  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxRawhitClusterListv1(const unsigned int length = SVXNRAWHITCLUSTER);
  virtual ~SvxRawhitClusterListv1();

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset    ();
  int  isValid  () const;
  void identify (std::ostream &os=std::cout) const;

  // Add/remove/set/get methods...
  // """""""""""""""""""""""""""""
  SvxRawhitCluster* addRawhitCluster  (const          int ihit = -1);
  void            removeRawhitCluster (const unsigned int ihit     );

  SvxRawhitCluster* get_RawhitCluster (const unsigned int ihit) const
    { return (SvxRawhitCluster*) RawhitCluster->UncheckedAt(ihit) ;}
  int             get_nRawhitClusters ()                        const
    { return (int) nRawhitClusters                                ;}
  bool             check_rawhitSorted ()                        const
    { return rawhitSorted                                         ;}
  bool            check_clusterSorted ()                        const
    { return clusterSorted                                        ;}
  // Routines to manipulate the rawhit2cluster array...
  // """"""""""""""""""""""""""""""""""""""""""""""""""
  int Compress();
  int set_TClonesArraySize(const unsigned int nhit);

  // Sorting and index search
  // """"""""""""""""""""""""
  void sortRawhits () ;
  void sortClusters() ;
  void unSort      () ;
  // Find first occurrence of rawhit
//  int  indexOfRawhit  (const int id        ,
//		       const int ifrom =  0,
//		       const int iupto = -1) const;
  // Find first occurrence of cluster
//  int  indexOfCluster (const int id        ,
//		       const int ifrom =  0,
//		       const int iupto = -1) const;
  bool indexOfRawhit(const int id, int& idx_lb, int& idx_ub, 
                     int ifrom=0, int iupto=-1) const;
  bool indexOfCluster(const int id, int& idx_lb, int& idx_ub, 
                     int ifrom=0, int iupto=-1) const;
  
  // Methods
  // """""""
  void print() const;

  // ---
  ClassDef(SvxRawhitClusterListv1,1)
};
#endif /* __SVXRAWHITCLUSTERLISTV1_HH_ */
