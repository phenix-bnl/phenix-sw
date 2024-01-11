// ==============
// FILE: SvxHit.h
// ==============

#ifndef __SVXHIT_HH_
#define __SVXHIT_HH_

#include <iostream>
#include "PHObject.h"
#include "phool.h"

/**
 * @brief  Base class for SVX hits: SvxGhit, SvxRawhit and SvxCluster.
 *
 * Functionalities basic and common to all the hits are implemented here, like get/set_hitID().
 *
 * Created  by V. L. Rykov on 06-Feb-2004
 * Modified by V. L. Rykov on 05-Apr-2004
 *      Comments to svxSection: : 0 - Barrel, 1 - North; 2 - South
 * Modified by V. L. Rykov on 09-May-2004: Sorting flag and methods added.
 */
class SvxHit : public PHObject
{

 public:

  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxHit(SvxHit *hit = NULL);  
  virtual ~SvxHit() {/*std::cout << "SvxHit object destroyed" << std::endl;*/}

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  virtual void Reset   ()                                ;
  virtual int  isValid ()                           const;
  virtual void identify(std::ostream &os=std::cout) const;

  // Set the data members
  // """"""""""""""""""""
  virtual void set_hitID      (const int val) { hitID      =         val ;}
  virtual void set_svxSection (const int val) { svxSection = (short) val ;}
  virtual void set_layer      (const int val) { layer      = (short) val ;}
  virtual void set_ladder     (const int val) { ladder     = (short) val ;}
  virtual void set_sensor     (const int val) { sensor     = (short) val ;}

  // Get the data members
  // """"""""""""""""""""
  virtual int get_hitID      () const { return       hitID      ;}
  virtual int get_svxSection () const { return (int) svxSection ;} ///< See svxSection.
  virtual int get_layer      () const { return (int) layer      ;}
  virtual int get_ladder     () const { return (int) ladder     ;}
  virtual int get_sensor     () const { return (int) sensor     ;}

  // Sortability
  virtual void   set_sortingSwitch (const int val) { sorting_switch = val; }
  virtual Bool_t IsSortable        ()                   const;
  virtual Int_t  Compare           (const TObject* hit) const;
  static bool CompPointer(const SvxHit* a, const SvxHit* b) { 
     return a->Compare(b) < 0;
  }

  // Methods
  // """""""
  virtual bool check_hitID(SvxHit* hit) const;
  virtual bool check_sensorID(SvxHit* hit) const;
  virtual SvxHit* Clone() { return new SvxHit(this); }
  virtual TObject* Clone(const char*) const {
    PHOOL_VIRTUAL_WARNING;
    return 0;
  }
  virtual void Copy(SvxHit* hit);
  virtual void Copy(TObject& object) const { PHOOL_VIRTUAL_WARNING; }
  virtual void print() const;

 protected:

  // Data member definition
  // """"""""""""""""""""""
  int   hitID;            ///< Hit number in the event
  // SVX sensor ID
  short svxSection;       ///< SVX section: 0 - Barrel; 1 - North; 2 - South
  short layer;            ///< layer number
  short ladder;           ///< ladder number
  short sensor;           ///< sensor number

  // Sorting flag
  static short sorting_switch; ///< -1: sort hitID; +1: sort sensorID

  ClassDef(SvxHit,1)

};

#endif
