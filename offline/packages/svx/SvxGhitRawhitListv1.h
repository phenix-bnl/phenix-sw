// ===========================
// FILE: SvxGhitRawhitListv1.h
// ===========================
// ****************************************************************************
// Container for the SvxGhitRawhitv1
// This contains all SvxGhitRawhit objects in an event.
// ---
// Created  by V. L. Rykov on 12-Feb-2004
//
// Modified by V. L. Rykov on 11-May-2004: Sorting and index search added.
// ****************************************************************************

#ifndef __SVXGHITRAWHITLISTV1_HH_
#define __SVXGHITRAWHITLISTV1_HH_

#include "SvxGhitRawhitList.h"
#include "TClonesArray.h"
#include "SvxGhitRawhitv1.h"

class SvxGhitRawhitListv1 : public SvxGhitRawhitList
{
 private:
  enum {SVXNGHITRAWHIT = 20000};   //! Default initial container size

 protected:
  // Data
  unsigned int  nGhitRawhits    ; //  Number of hits in the container
  TClonesArray* GhitRawhit      ; //  Hit container
  bool          ghitSorted      ; //! Sorting flag for ghits
  bool          rawhitSorted    ; //! Sorting flag for rawhits
  bool          listPointersSet ; //! Each SvxGhitRawhitv1 knows list pointer
  // Methods
  void SortSTL();
  void restoreListPointers(); // Give list pointer to each SvxGhitRawhitv1

 public:
  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxGhitRawhitListv1(const unsigned int length = SVXNGHITRAWHIT);
  virtual ~SvxGhitRawhitListv1();

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset    ();
  int  isValid  () const;
  void identify (std::ostream &os=std::cout) const;

  // Add/remove/set/get methods...
  // """""""""""""""""""""""""""""
  SvxGhitRawhit* addGhitRawhit    (const          int ihit = -1);
  void            removeGhitRawhit (const unsigned int ihit     );

  SvxGhitRawhit* get_GhitRawhit   (const unsigned int ihit) const
    { return (SvxGhitRawhit*) GhitRawhit->UncheckedAt(ihit) ;}
  int            get_nGhitRawhits ()                        const
    { return (int) nGhitRawhits                             ;}
  bool           check_ghitSorted ()                        const
    { return ghitSorted                                     ;}
  bool         check_rawhitSorted ()                        const
    { return rawhitSorted                                   ;}
 
  // Routines to manipulate the rawhit array...
  // """"""""""""""""""""""""""""""""""""""""""
  int Compress();
  int set_TClonesArraySize(const unsigned int nhit);

  // Sorting and index search
  // """"""""""""""""""""""""
  void sortGhits   () ;
  void sortRawhits () ;
  void unSort      () ;

  // Find first occurrence of ghit
//  int  indexOfGhit   (const int id        ,
//		      const int ifrom =  0,
//		      const int iupto = -1) const;
  // Find first occurrence of rawhit
//  int  indexOfRawhit (const int id        ,
//		      const int ifrom =  0,
//		      const int iupto = -1) const;
  bool indexOfGhit(const int id, int& idx_lb, int& idx_ub, 
                     int ifrom=0, int iupto=-1) const;
  bool indexOfRawhit(const int id, int& idx_lb, int& idx_ub, 
                     int ifrom=0, int iupto=-1) const;
  
  // Methods
  // """""""
  void print() const;

  // ---
  ClassDef(SvxGhitRawhitListv1,1)
};
#endif /* __SVXGHITRAWHITLISTV1_HH_ */
