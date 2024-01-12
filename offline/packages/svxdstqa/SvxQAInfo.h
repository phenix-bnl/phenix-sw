#ifndef __SVXQAINFO_HH_
#define __SVXQAINFO_HH_

#include <phool.h>
#include <PHObject.h>

class SvxQAInfo : public PHObject
{

 public:

  SvxQAInfo(SvxQAInfo* info = NULL) {}
  virtual ~SvxQAInfo() {}

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxQAInfo object" << std::endl;
  }

  /// Clear Event
  virtual void Reset(){ PHOOL_VIRTUAL_WARN("Reset");}

  /// isValid returns non zero if object contains vailid data
  virtual int isValid() const {
    PHOOL_VIRTUAL_WARN("isValid");
    return -9999;
  }

  // Set the values in the SvxQAInfo...
  // """"""""""""""""""""""""""""""""""
  virtual void set_CellStuckEvent(const int module, const int val) {
    PHOOL_VIRTUAL_WARN("stripCellID");
  }

  // Get the values from the SvxQAInfo...
  // """"""""""""""""""""""""""""""""""""
  virtual int get_CellStuckEvent(const int module) const {
    PHOOL_VIRTUAL_WARN("stripCellID");
    return -9999;
  }

  // Methods
  // """""""
  virtual SvxQAInfo* Clone(const char* = "") const {
    PHOOL_VIRTUAL_WARN("Clone()");
    return 0;
  }
  virtual void Copy(TObject& object) const { PHOOL_VIRTUAL_WARN("Copy()"); }

  ClassDef(SvxQAInfo, 1);
};
#endif
