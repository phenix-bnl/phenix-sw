// ========================
// FILE: SvxGhitClusterv1.h
// ========================

#ifndef __SVXGHITCLUSTERV1_HH_
#define __SVXGHITCLUSTERV1_HH_

#include "SvxGhitCluster.h"
#include "SvxGhitClusterList.h"

// ***********************************************************************
// Relater of SVX GEANT hits to SVX clusters
// ---
// Created  by V. L. Rykov on 15-Feb-2004
//
// Modified by V. L. Rykov on 09-May-2004: Sorting flag and methods added.
// ***********************************************************************

class SvxGhitClusterv1 : public SvxGhitCluster
{
 public:

  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxGhitClusterv1(SvxGhitClusterList* g2c_list    = NULL,
		   SvxGhitCluster*     ghitcluster = NULL);
  SvxGhitClusterv1(SvxGhitCluster*     ghitcluster       );

  virtual ~SvxGhitClusterv1()
    {/*std::cout << "SvxGhitClusterv1 object destroyed" << std::endl;*/}

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset   ();
  int  isValid ()                           const;
  void identify(std::ostream &os=std::cout) const;

  // Set data members
  // """"""""""""""""
  void set_ghitID    (const int val) {
    ghitID = val ;
    if ( g2cList ) g2cList->unSort();
  }
  void set_clusterID (const int val) {
    clusterID = val ;
    if ( g2cList ) g2cList->unSort();
  }

  // Get data members
  // """"""""""""""""
  int get_ghitID    () const { return    ghitID ;}
  int get_clusterID () const { return clusterID ;}

  // Sortability
  void set_ListPointer    (SvxGhitClusterList* list) { g2cList = list; }
  void   set_sortingSwitch(const int val) { sorting_switch = val; }
  Bool_t IsSortable       ()                            const;
  //Int_t  Compare          (const PHObject* ghit2cluster) const;
  Int_t  Compare          (const TObject* ghit2cluster) const;

  // Methods
  // """""""
  virtual SvxGhitCluster* Clone() { return new SvxGhitClusterv1(this); }
  virtual void Copy(SvxGhitCluster* hit);
  void print() const;

 protected:

  // Data member definition
  int    ghitID;               // Ghit ID
  int clusterID;               // Cluster ID

  SvxGhitClusterList* g2cList; //! Pointer to the container

  // Sorting flags
  static short sorting_switch; // -1: sort ghits; +1: sort clusters

  ClassDef(SvxGhitClusterv1,1)
};
#endif
