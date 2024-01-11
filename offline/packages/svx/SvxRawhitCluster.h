#ifndef __SVXRAWHITCLUSTER_HH_
#define __SVXRAWHITCLUSTER_HH_

#include <iostream>
#include <phool.h>
#include <PHObject.h>

/**
 * @brief  The abstract class for the SVX rawhit<->clusters relater
 *
 * @date  Created  by V. L. Rykov on 15-Feb-2004
 * @date  Modified by V. L. Rykov on 08-May-2004: Sorting flag and methods added.
 */
class SvxRawhitCluster : public PHObject
{
 public:
  SvxRawhitCluster() {}
  virtual ~SvxRawhitCluster() {}

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void Reset() { PHOOL_VIRTUAL_WARN("void Reset()"); }
  virtual int isValid() const {
    PHOOL_VIRTUAL_WARN("int isValid() const");
    return 0;
  }
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxRawhitCluster object" << std::endl;
  }

  // Set the values in the SvxRawhitCluster...
  // """""""""""""""""""""""""""""""""""""""""
  virtual void set_rawhitID(const int val) {
    PHOOL_VIRTUAL_WARN("set_rawhitID");
  }
  virtual void set_clusterID(const int val) {
    PHOOL_VIRTUAL_WARN("set_clusterID");
  }

  // Get the values from the SvxRawhitCluster...
  // """""""""""""""""""""""""""""""""""""""""""
  virtual int get_rawhitID() const {
    PHOOL_VIRTUAL_WARN("get_rawhitID");
    return -9999;
  }
  virtual int get_clusterID() const {
    PHOOL_VIRTUAL_WARN("get_clusterID");
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
  // virtual Int_t Compare          (const PHObject* rawhit2cluster) const
  //  { PHOOL_VIRTUAL_WARN("Int_t Compare(const PHObject*) const"); return 0; }
  virtual Int_t Compare(const TObject* rawhit2cluster) const {
    PHOOL_VIRTUAL_WARN("Int_t Compare(const TObject*) const");
    return 0;
  }
  static bool CompPointer(const SvxRawhitCluster* a, const SvxRawhitCluster* b) {
     return a->Compare(b) < 0;
  }

  // Methods
  // """""""
  virtual SvxRawhitCluster* Clone() {
    PHOOL_VIRTUAL_WARN("Clone()");
    return 0;
  }
  virtual TObject* Clone(const char* newname = "") const {
    PHOOL_VIRTUAL_WARNING;
    return 0;
  }
  virtual void Copy(SvxRawhitCluster* hit) { PHOOL_VIRTUAL_WARN("Copy()"); }
  virtual void Copy(TObject& object) const { PHOOL_VIRTUAL_WARNING; }
  virtual void print() const { PHOOL_VIRTUAL_WARN("void print()"); }

  ClassDef(SvxRawhitCluster, 1);
};
#endif
