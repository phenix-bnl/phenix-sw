// ==========================
// FILE: SvxRawhitClusterv1.h
// ==========================

#ifndef __SVXRAWHITCLUSTERV1_HH_
#define __SVXRAWHITCLUSTERV1_HH_

#include "SvxRawhitCluster.h"
#include "SvxRawhitClusterList.h"

// ***********************************************************************
// Relater of SVX raw hits to SVX clusters
// ---
// Created  by V. L. Rykov on 15-Feb-2004
//
// Modified by V. L. Rykov on 09-May-2004: Sorting flag and methods added.
// ***********************************************************************

class SvxRawhitClusterv1 : public SvxRawhitCluster
{
 public:

  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxRawhitClusterv1(SvxRawhitClusterList* r2c_list      = NULL,
		     SvxRawhitCluster*     rawhitcluster = NULL);
  SvxRawhitClusterv1(SvxRawhitCluster*     rawhitcluster       );

  virtual ~SvxRawhitClusterv1()
    {/*std::cout << "SvxRawhitClusterv1 object destroyed" << std::endl;*/}

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset   ();
  int  isValid ()                           const;
  void identify(std::ostream &os=std::cout) const;

  // Set data members
  // """"""""""""""""
  void set_rawhitID  (const int val) {
    rawhitID  = val ;
    if ( r2cList ) r2cList->unSort();
  }
  void set_clusterID (const int val) {
    clusterID = val ;
    if ( r2cList ) r2cList->unSort();
  }

  // Get data members
  // """"""""""""""""
  int get_rawhitID  () const { return  rawhitID ;}
  int get_clusterID () const { return clusterID ;}

  // Sortability
  void set_ListPointer    (SvxRawhitClusterList* list) { r2cList = list; }
  void   set_sortingSwitch(const int val) { sorting_switch = val; }
  Bool_t IsSortable       ()                              const;
  //Int_t  Compare          (const PHObject* rawhit2cluster) const;
  Int_t  Compare          (const TObject* rawhit2cluster) const;

  // Methods
  // """""""
  virtual SvxRawhitCluster* Clone() { return new SvxRawhitClusterv1(this); }
  virtual void Copy(SvxRawhitCluster* hit);
  void print() const;

 protected:

  // Data member definition
  int  rawhitID;                 // Rawhit  ID
  int clusterID;                 // Cluster ID

  SvxRawhitClusterList* r2cList; //! Pointer to the container

  // Sorting flags
  static short sorting_switch;   // -1: sort rawhits; +1: sort clusters

  ClassDef(SvxRawhitClusterv1,1)
};
#endif
