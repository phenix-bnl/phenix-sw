// =================
// FILE: SvxEventInfov1.h
// =================

#ifndef __SVXEVENTINFOV1_HH_
#define __SVXEVENTINFOV1_HH_

/**
 * @brief  Event information from svx packet
 *
 * This will be used for identifying svx error status during DAQ.
 * @date  Created by T. Hachiya on Jun 21 2011
 */

#include <SvxEventInfo.h>
#include <SvxParameters.h>

class SvxEventInfov1 : public SvxEventInfo
{
 public:
  SvxEventInfov1(SvxEventInfo* info = NULL);
  virtual ~SvxEventInfov1(){}

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxEventInfo object" << std::endl;
  }

  /// Clear Event
  virtual void Reset();

  /// isValid returns non zero if object contains vailid data
  //virtual int isValid() const {PHOOL_VIRTUAL_WARN("isValid"); return -9999;}


  // Set the values in the SvxEventInfo...
  // """"""""""""""""""""""""""""""""""
  virtual void set_pixelStatus (const int module, const int val);
  virtual void set_stripCellID (const int module, const int chip, const int val);
  virtual void set_stripStatus (const int module, const int val);

  // Get the values from the SvxEventInfo...
  // """"""""""""""""""""""""""""""""""""
  virtual int get_pixelStatus(const int module) const;
  virtual int get_stripCellID(const int module, const int chip) const;
  virtual int get_stripStatus(const int module) const;

  // Methods
  // """""""

 private:
  //////////////////
  int m_pixelStatus[SVXNMODULEPIXEL];
  int m_stripCellID[SVXNMODULESTRIP][6];
  int m_stripStatus[SVXNMODULESTRIP];

  //---
  ClassDef(SvxEventInfov1,1)

};
#endif
