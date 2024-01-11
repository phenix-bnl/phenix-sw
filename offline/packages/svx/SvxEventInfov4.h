// =================
// FILE: SvxEventInfov4.h
// =================

#ifndef __SVXEVENTINFOV4_HH_
#define __SVXEVENTINFOV4_HH_

/**
 * @brief  Event information from svx packet
 *
 * This will be used for identifying svx error status during DAQ.
 * @date  Created by T. Hachiya on Oct 21 2013
 */

#include <SvxEventInfo.h>
#include <SvxParameters.h>

class Event;
class Packet;

class SvxEventInfov4 : public SvxEventInfo
{
 public:
  SvxEventInfov4(SvxEventInfo* info = NULL);
  virtual ~SvxEventInfov4(){}

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxEventInfov4 object" << std::endl;
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
  virtual void set_stripCellID (const int module, const int sensor, const int chip, const unsigned char val);
  virtual void set_pixelNRawhits(const int ladder, const int sensor, const int nrawhits);
  virtual void set_pixelNClusters(const int ladder, const int sensor, const int nclusters);
  virtual void set_stripNRawhits(const int module, const int nrawhits);
  virtual void set_stripNClusters(const int module, const int nclusters);

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
  virtual unsigned char get_stripCellID (const int module, const int sensor, const int chip) const;
  virtual unsigned char get_pixelNRawhits(const int ladder, const int sensor) const;
  virtual unsigned char get_pixelNClusters(const int ladder, const int sensor) const;
  virtual unsigned char get_stripNRawhits(const int module) const;
  virtual unsigned char get_stripNClusters(const int module) const;
  
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
  unsigned char m_pixelNRawhits[SVXLADDERSLAYER0*2+SVXLADDERSLAYER1*2][SVXSENSORSLAYER0];
  unsigned char m_pixelNClusters[SVXLADDERSLAYER0*2+SVXLADDERSLAYER1*2][SVXSENSORSLAYER0];

  int m_stripStatus   [SVXNMODULESTRIP];
  int m_stripSubsysErr[SVXNMODULESTRIP];
  int m_stripDcmErr   [SVXNMODULESTRIP];
  int m_stripBeamClock[SVXNMODULESTRIP];
  int m_stripEvtCount [SVXNMODULESTRIP];  
  unsigned char m_stripCellID[SVXNMODULESTRIP][6][12];
  unsigned char m_stripNRawhits[SVXNMODULESTRIP];
  unsigned char m_stripNClusters[SVXNMODULESTRIP];
  //---
  ClassDef(SvxEventInfov4,1)

};
#endif
