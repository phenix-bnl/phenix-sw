// ========================
// FILE: SvxClusterListv5.h
// ========================
// ****************************************************************************
// Container for the SvxClusterv5
// This contains a list of all SvxCluster objects in an event.
// ---
// Created  by Sasha Lebedev <lebedev@iastate.edu> in December 2010
//
// ****************************************************************************

#ifndef __SVXCLUSTERLISTV5_HH_
#define __SVXCLUSTERLISTV5_HH_

#include "TClonesArray.h"
#include "SvxClusterList.h"
#include "SvxClusterv5.h"

class SvxClusterListv5 : public SvxClusterList
{
 private:
  enum {SVXNCLUSTER = 20000};   //! Default initial size of Cluster list

 protected:
  TClonesArray* m_hit_list      ; // Hit container
  bool          hitIDsorted     ; //! clusters are sorted, using hitID
  bool          sensorIDsorted  ; //! clusters are sorted, using sensorID
  bool          listPointersSet ; //! Each SvxClusterv5 knows list pointer
  int           m_hit_id_unused ; ///< Smallest hit ID not used.

  // Methods
  void restoreListPointers();     // Supply list pointer to each SvxClusterv5

 public:
  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxClusterListv5(const unsigned int length = SVXNCLUSTER);
  virtual ~SvxClusterListv5();

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset    ();
  int  isValid  () const;
  void identify (std::ostream &os=std::cout) const;

  // Add/remove/set/get methods...
  // """""""""""""""""""""""""""""
  SvxCluster* addCluster    (const          int ihit = -1);
  void        removeCluster (const unsigned int ihit     );

  SvxCluster* get_Cluster   (const unsigned int ihit) const
    { return (SvxCluster*) m_hit_list->UncheckedAt(ihit) ;}
  int         get_nClusters     ()                    const
    { return m_hit_list->GetLast() + 1 ;}
  bool     check_hitIDsorted    ()                    const
    { return hitIDsorted                              ;}
  bool     check_sensorIDsorted ()                    const
    { return sensorIDsorted                           ;}
 
  // Routines to manipulate the cluster array...
  // """""""""""""""""""""""""""""""""""""""""""
  int Compress();
  int set_TClonesArraySize(const unsigned int nhit);
  
  // Sorting and index search
  // """"""""""""""""""""""""
  void sort_hitID    () ;
  void sort_sensorID () ;
  void unSort        () ;
  void SortSTL       () ;

  // Find first occurrence of cluster
  int indexOfCluster (const int         hitid    ,
		      int         ifrom = 0,
		      int         iupto =-1) const;
//  int indexOfCluster (const SvxSensor*  sensor   ,
//		      const int         ifrom = 0,
//		      const int         iupto =-1) const;
  int indexOfCluster (const SvxCluster* hit      ,
		      const int         ifrom = 0,
		      const int         iupto =-1) const;
  bool indexOfCluster(const SvxSensor* sensor, 
                      int& idx_lb, int& idx_ub, 
                      int ifrom = 0, int iupto =-1) const;

  // Methods
  // """""""
  void print() const;

  //---
  ClassDef(SvxClusterListv5,1)
};
#endif 



