// =======================
// FILE: SvxGhitRawhitv1.h
// =======================

#ifndef __SVXGHITRAWHITV1_HH_
#define __SVXGHITRAWHITV1_HH_

#include "SvxGhitRawhit.h"
#include "SvxGhitRawhitList.h"

// ***********************************************************************
// Relater of SVX GEANT hits to SVX raw hits.
// ---
// Created  by V. L. Rykov on 09-Feb-2004
//
// Modified by V. L. Rykov on 08-May-2004: Sorting flag and methods added.
// ***********************************************************************

class SvxGhitRawhitv1 : public SvxGhitRawhit
{
 public:

  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxGhitRawhitv1(SvxGhitRawhitList* g2r_list   = NULL,
		  SvxGhitRawhit*     ghitrawhit = NULL);
  SvxGhitRawhitv1(SvxGhitRawhit*     ghitrawhit       );

  virtual ~SvxGhitRawhitv1()
    {/*std::cout << "SvxGhitRawhitv1 object destroyed" << std::endl;*/}

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset   ();
  int  isValid ()                           const;
  void identify(std::ostream &os=std::cout) const;

  // Set data members
  // """"""""""""""""
  void set_ghitID   (const int val) {
    ghitID   = val ;
    if ( g2rList ) g2rList->unSort();
  }
  void set_rawhitID (const int val) {
    rawhitID = val ;
    if ( g2rList ) g2rList->unSort();
  }

  // Get data members
  // """"""""""""""""
  int get_ghitID   () const { return   ghitID ;}
  int get_rawhitID () const { return rawhitID ;}

  // Sortability
  void   set_ListPointer    (SvxGhitRawhitList* list) { g2rList = list; }
  void   set_sortingSwitch(const int val) { sorting_switch = val; }
  Bool_t IsSortable       ()                           const;
  Int_t  Compare          (const TObject* ghit2rawhit) const;

  // Methods
  // """""""
  virtual SvxGhitRawhit* Clone() { return new SvxGhitRawhitv1(this); }
  virtual void Copy(SvxGhitRawhit* hit);
  void print() const;

 protected:

  // Data member definition
  int   ghitID;                // Ghit ID
  int rawhitID;                // Rawhit ID

  SvxGhitRawhitList* g2rList;  //! Pointer to the container

  // Sorting flag
  static short sorting_switch; // -1: sort ghits; +1: sort rawhits

  ClassDef(SvxGhitRawhitv1,1)
};
#endif
