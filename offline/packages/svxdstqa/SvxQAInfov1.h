#ifndef __SVXQAINFOV1_HH_
#define __SVXQAINFOV1_HH_

#include <SvxQAInfo.h>
#include <SvxParameters.h>

class SvxQAInfov1 : public SvxQAInfo
{
 public:
  SvxQAInfov1(SvxQAInfo* info = NULL);
  virtual ~SvxQAInfov1(){}

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxQAInfo object" << std::endl;
  }

  /// Clear Event
  virtual void Reset();

  /// isValid returns non zero if object contains vailid data
  //virtual int isValid() const {warning("isValid"); return -9999;}


  // Set the values in the SvxQAInfo...
  // """"""""""""""""""""""""""""""""""

  virtual void set_CellStuckEvent(const int module, const int val);


  // Get the values from the SvxQAInfo...
  // """"""""""""""""""""""""""""""""""""

  virtual int get_CellStuckEvent(const int module) const;


  // Methods
  // """""""

 private:
  //////////////////
  int m_CellStuckEvent[SVXNMODULESTRIP];

  //---
  ClassDef(SvxQAInfov1,1)

};
#endif
