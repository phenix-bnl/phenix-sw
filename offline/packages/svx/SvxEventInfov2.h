// =================
// FILE: SvxEventInfov2.h
// =================

#ifndef __SVXEVENTINFOV2_HH_
#define __SVXEVENTINFOV2_HH_

/**
 * @brief  Event information from svx packet
 *
 * This will be used for identifying svx error status during DAQ.
 * @date  Created by R. Akimoto on Mar 19 2012
 */

#include <SvxEventInfo.h>
#include <SvxParameters.h>

class Event;
class Packet;

class SvxEventInfov2 : public SvxEventInfo
{
 public:
  SvxEventInfov2(SvxEventInfo* info = NULL);
  virtual ~SvxEventInfov2(){}

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
  virtual void set_GlobalInfo (Event *e);

  virtual void set_pixelStatus (const int module, const int val);
  virtual void set_stripStatus (const int module, const int val);
  virtual void set_pixelStatus (const int module, Packet *p);
  virtual void set_stripStatus (const int module, Packet *p);
  virtual void set_stripCellID (const int module, const int chip, const int val);

  // Get the values from the SvxEventInfo...
  // """"""""""""""""""""""""""""""""""""
  virtual int get_eventID       (void) const { return m_event; }
  virtual int get_GL1ClockCount (void) const { return m_gl1clk_count; }
  virtual int get_pixelStatus (const int module) const;
  virtual int get_stripStatus (const int module) const;
  virtual int get_pixelSubsysStatus (const int module) const;
  virtual int get_stripSubsysStatus (const int module) const;
  virtual int get_pixelDcmStatus (const int module) const;
  virtual int get_stripDcmStatus (const int module) const;
  virtual int get_pixelBeamClock (const int module) const;
  virtual int get_stripBeamClock (const int module) const;
  virtual int get_pixelEventCount (const int module) const;
  virtual int get_stripEventCount (const int module) const;
  virtual int get_stripCellID (const int module, const int chip) const;

  // Methods
  // """""""

 private:
  //////////////////
  int m_event;
  int m_gl1clk_count;

  int m_pixelStatus   [SVXNMODULEPIXEL];
  int m_pixelSubsysErr[SVXNMODULEPIXEL];
  int m_pixelDcmErr   [SVXNMODULEPIXEL];
  int m_pixelBeamClock[SVXNMODULEPIXEL];
  int m_pixelEvtCount [SVXNMODULEPIXEL];

  int m_stripStatus   [SVXNMODULESTRIP];
  int m_stripSubsysErr[SVXNMODULESTRIP];
  int m_stripDcmErr   [SVXNMODULESTRIP];
  int m_stripBeamClock[SVXNMODULESTRIP];
  int m_stripEvtCount [SVXNMODULESTRIP];  
  int m_stripCellID[SVXNMODULESTRIP][6];

  //---
  ClassDef(SvxEventInfov2,1)

};
#endif
