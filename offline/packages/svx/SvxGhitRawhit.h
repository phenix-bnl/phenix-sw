// =====================
// FILE: SvxGhitRawhit.h
// =====================

#ifndef __SVXGHITRAWHIT_HH_
#define __SVXGHITRAWHIT_HH_

#include <phool.h>
#include <PHObject.h>

#include <iostream>

/**
 * @brief  The abstract class for the SVX ghit<->rawhit relater
 *
 * @date  Created  by V. L. Rykov on 09-Feb-2004
 * @date  Modified by V. L. Rykov on 08-May-2004: Sorting flag and methods added.
 */
class SvxGhitRawhit : public PHObject
{

 public:
  SvxGhitRawhit()
    {/*std::cout << "SvxGhitRawhit virtual object created" << std::endl;*/}
  virtual ~SvxGhitRawhit() {
    //std::cout << "SvxGhitRawhit virtual object destroyed" << std::endl;
  }

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void Reset() { PHOOL_VIRTUAL_WARN("void Reset()"); }
  virtual int isValid() const {
    PHOOL_VIRTUAL_WARN("int isValid() const");
    return 0;
  }
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxGhitRawhit object" << std::endl;
  }

  // Set the values in the SvxGhitRawhit...
  // """"""""""""""""""""""""""""""""""""""
  virtual void set_ghitID(const int val) { PHOOL_VIRTUAL_WARN("set_ghitID"); }
  virtual void set_rawhitID(const int val) {
    PHOOL_VIRTUAL_WARN("set_rawhitID");
  }

  // Get the values from the SvxGhitRawhit...
  // """"""""""""""""""""""""""""""""""""""""
  virtual int get_ghitID() const {
    PHOOL_VIRTUAL_WARN("get_ghitID");
    return -9999;
  }
  virtual int get_rawhitID() const {
    PHOOL_VIRTUAL_WARN("get_rawhitID");
    return -9999;
  }

  // Sortability
  virtual void set_sortingSwitch(const int val) {
    PHOOL_VIRTUAL_WARN("void set_sortingSwitch(const int val)");
  }
  virtual Bool_t IsSortable() const {
    PHOOL_VIRTUAL_WARN("Bool_t IsSortable() const");
    return false;
  }
  // virtual Int_t Compare          (const PHObject* ghit2rawhit) const
  //  { PHOOL_VIRTUAL_WARN("Int_t Compare(const PHObject*) const"); return 0; }
  virtual Int_t Compare(const TObject* ghit2rawhit) const {
    PHOOL_VIRTUAL_WARN("Int_t Compare(const TObject*) const");
    return 0;
  }
  static bool CompPointer(const SvxGhitRawhit* a, const SvxGhitRawhit* b) {
     return a->Compare(b) < 0;
  }

  // Methods
  // """""""
  virtual SvxGhitRawhit* Clone() {
    PHOOL_VIRTUAL_WARN("Clone()");
    return 0;
  }
  virtual TObject* Clone(const char* newname = "") const {
    PHOOL_VIRTUAL_WARNING;
    return 0;
  }
  virtual void Copy(SvxGhitRawhit* hit) { PHOOL_VIRTUAL_WARN("Copy()"); }
  virtual void Copy(TObject& object) const { PHOOL_VIRTUAL_WARNING; }
  virtual void print() const { PHOOL_VIRTUAL_WARN("void print()"); }

  ClassDef(SvxGhitRawhit, 1);
};
#endif
