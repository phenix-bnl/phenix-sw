// =======================
// FILE: SvxRawhitListv1.h
// =======================
// ****************************************************************************
// Container for the SvxRawhitv1
// This contains a list of all SvxRawhit objects in an event.
// Template used: SvxGhitListv1.h by Jeffery Mitchell as of 11/20/2003
// ---
// Created  by V. L. Rykov on 09-Mar-2004
//
// Modified by V. L. Rykov on 13-May-2004: Sorting and index search added.
// ****************************************************************************

#ifndef __SVXRAWHITLISTV1_HH_
#define __SVXRAWHITLISTV1_HH_

#include "SvxRawhitList.h"
#include "TClonesArray.h"
#include "SvxRawhitv1.h"

class SvxRawhitListv1 : public SvxRawhitList
{
 private:
  enum {SVXNRAWHIT = 20000};   //! Default initial size of Rawhit list

 protected:
  TClonesArray* m_hit_list      ; // Hit container
  bool          hitIDsorted     ; //! rawhits are sorted, using hitID
  bool          sensorIDsorted  ; //! rawhits are sorted, using sensorID
  bool          listPointersSet ; //! Each SvxRawhitv1 knows list pointer
  int           m_hit_id_unused ; ///< Smallest hit ID not used.

  // Methods
  void SortSTL();
  void restoreListPointers();     // Supply list pointer to each SvxRawhitv1

 public:
  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxRawhitListv1(const unsigned int length = SVXNRAWHIT);

  virtual ~SvxRawhitListv1();

  // Copy class
//  virtual SvxRawhitListv1* clone() const {
//    return new SvxRawhitListv1(*this);
//  }

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  void Reset    ();
  int  isValid  () const;
  void identify (std::ostream &os=std::cout) const;

  // Add/remove/set/get methods...
  // """""""""""""""""""""""""""""
  SvxRawhit* addRawhit    (const          int ihit = -1);
  void       removeRawhit (const unsigned int ihit     );

  SvxRawhit* get_Rawhit   (const unsigned int ihit) const
//    { return (SvxRawhit*) m_hit_list->UncheckedAt(ihit) ;}
    { return dynamic_cast<SvxRawhit*>(m_hit_list->UncheckedAt(ihit)) ;}
  int        get_nRawhits       ()                  const
    { return m_hit_list->GetLast() + 1 ;}
  bool     check_hitIDsorted    ()                  const
    { return hitIDsorted                        ;}
  bool     check_sensorIDsorted ()                  const
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

  // Find first occurrence of rawhit
  int indexOfRawhit (const int        hitid    ,
		     int        ifrom = 0,
		     int        iupto =-1) const;
//  int indexOfRawhit (const SvxSensor* sensor   ,
//		     const int        ifrom = 0,
//		     const int        iupto =-1) const;
  // SvxRawhit of the same sensor (npar=0) && sensorSection (npar=1)
  //                                       && sensorReadout (npar=2)
  //                                       && channel       (npar=3) 
  int indexOfRawhit (      SvxRawhit& rawhit   ,
		     const int        npar  = 3,
		     const int        ifrom = 0,
		     const int        iupto =-1) const;
  // SvxRawhit having the pointer equal to hit
  int indexOfRawhit (const SvxRawhit* hit      ,
		     const int        ifrom = 0,
		     const int        iupto =-1) const;
  bool indexOfRawhit(const SvxSensor* sensor, 
                     int& idx_lb, int& idx_ub, 
                     int ifrom = 0, int iupto =-1) const;
  
  // Methods
  // """""""
  void print() const;

  //---
  ClassDef(SvxRawhitListv1,1)
};
#endif /* __SVXRAWHITLISTV1_HH_ */
