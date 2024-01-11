// =====================
// FILE: SvxGhitListv1.h
// =====================
// ****************************************************************************
// Container for the SvxGhitv1
// This contains a list of all SvxGhit objects in an event.
// Template used: SvxGhitListv1.h by Jeffery Mitchell as of 11/20/2003
// ---
// Created  by V. L. Rykov on 09-Mar-2004
//
// Modified by V. L. Rykov on 10-May-2004: Sorting and index search added.
// ****************************************************************************

#ifndef __SVXGHITLISTV1_HH_
#define __SVXGHITLISTV1_HH_

#include "SvxGhitList.h"
#include "TClonesArray.h"
#include "SvxGhitv1.h"

class SvxGhitListv1 : public SvxGhitList
{
 private:
  enum {SVXNGHIT = 20000}; //! Default initial size of Ghit list

 protected:
  TClonesArray* m_hit_list      ; // Hit container
  bool          hitIDsorted     ; //! ghits are sorted, using hitID
  bool          sensorIDsorted  ; //! ghits are sorted, using sensorID
  bool          listPointersSet ; //! Each SvxGhitv1 knows list pointer
  int           m_hit_id_unused ; ///< Smallest hit ID not used.

  // Methods
  void SortSTL();
  void restoreListPointers();     // Supply list pointer to each SvxGhitv1

 public:
  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxGhitListv1(const unsigned int length = SVXNGHIT);
  virtual ~SvxGhitListv1();

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset    ();
  int  isValid  () const;
  void identify (std::ostream &os=std::cout) const;

  // Add/remove/set/get methods...
  // """""""""""""""""""""""""""""
  SvxGhit* addGhit    (const          int ihit = -1);
  void     removeGhit (const unsigned int ihit     );

  SvxGhit* get_Ghit   (const unsigned int ihit) const
    { return (SvxGhit*) m_hit_list->UncheckedAt(ihit) ;}
  int      get_nGhits ()                        const
    { return m_hit_list->GetLast() + 1 ;}
  bool     check_hitIDsorted    ()              const
    { return hitIDsorted                        ;}
  bool     check_sensorIDsorted ()              const
    { return sensorIDsorted                     ;}
 
  // Routines to manipulate the cluster array...
  // """""""""""""""""""""""""""""""""""""""""""
  int Compress();
  int set_TClonesArraySize(const unsigned int nhit);
  
  // Sorting and index search
  // """"""""""""""""""""""""
  void sort_hitID    () ;
  void sort_sensorID () ;
  void unSort        () ;

  // Find first occurrence of ghit
  int indexOfGhit (const int        hitid    ,
		   int        ifrom = 0,
		   int        iupto =-1) const;
//  int indexOfGhit (const SvxSensor* sensor   ,
//		   const int        ifrom = 0,
//		   const int        iupto =-1) const;
  int indexOfGhit (const SvxGhit*   hit      ,
		   const int        ifrom = 0,
		   const int        iupto =-1) const;
  bool indexOfGhit(const SvxSensor* sensor, 
                   int& idx_lb, int& idx_ub, 
                   int ifrom = 0, int iupto =-1) const;
  // Methods
  // """""""
  void print() const;

  //---
  ClassDef(SvxGhitListv1,1)
};
#endif /* __SVXGHITLISTV1_HH_ */
