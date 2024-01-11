// =================
// FILE: SvxEventInfo.h
// =================

#ifndef __SVXEVENTINFO_HH_
#define __SVXEVENTINFO_HH_

/**
 * @brief  Event information from svx packet
 *
 * This will be used for identifying svx error status during DAQ.
 * @date  Created by T. Hachiya on Jun 21 2011
 */

#include <phool.h>
#include <PHObject.h>

class Event;
class Packet;

class SvxEventInfo : public PHObject
{
 public:
  SvxEventInfo(SvxEventInfo* info = NULL) 
    { /*std::cout << "SvxEventInfo object created" << std::endl;*/ }
  virtual ~SvxEventInfo()
    { /*std::cout << "SvxEventInfo object destroyed" << std::endl;*/ }

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxEventInfo object" << std::endl;
  }

  /// Clear Event
  virtual void Reset() { PHOOL_VIRTUAL_WARN("Reset"); }

  /// isValid returns non zero if object contains vailid data
  virtual int isValid() const {
    PHOOL_VIRTUAL_WARN("isValid");
    return -9999;
  }

  // Set the values in the SvxEventInfo...
  // """"""""""""""""""""""""""""""""""
  virtual void set_GlobalInfo(Event* e) { PHOOL_VIRTUAL_WARN("globalInfo"); }

  virtual void set_stripCellID(const int module, const int chip,
                               const int val) {
    PHOOL_VIRTUAL_WARN("stripCellID");
  }
  virtual void set_stripCellID(const int module, const int sensor,
                               const int chip, const unsigned char val) {
    PHOOL_VIRTUAL_WARN("stripCellID");
  }
  virtual void set_stripStatus(const int module, const int val) {
    PHOOL_VIRTUAL_WARN("stripStatus");
  }
  virtual void set_stripStatus(const int module, Packet* p) {
    PHOOL_VIRTUAL_WARN("stripStatus");
  }
  virtual void set_pixelStatus(const int module, const int val) {
    PHOOL_VIRTUAL_WARN("pixelStatus");
  }
  virtual void set_pixelStatus(const int module, Packet* p) {
    PHOOL_VIRTUAL_WARN("pixelStatus");
  }
  virtual void set_stripNRawhits(const int module, const int nrawhits) {
    PHOOL_VIRTUAL_WARN("stripRawInfo");
  }
  virtual void set_stripNClusters(const int module, const int nclusters) {
    PHOOL_VIRTUAL_WARN("stripNClusters");
  }
  virtual void set_pixelNRawhits(const int ladder, const int sensor,
                                 const int nrawhits) {
    PHOOL_VIRTUAL_WARN("pixelRawInfo");
  }
  virtual void set_pixelNClusters(const int ladder, const int sensor,
                                  const int nclusters) {
    PHOOL_VIRTUAL_WARN("pixelNClusters");
  }

  // Get the values from the SvxEventInfo...
  // """"""""""""""""""""""""""""""""""""
  virtual int get_eventID(void) const {
    PHOOL_VIRTUAL_WARN("eventID");
    return -9999;
  }
  virtual int get_GL1ClockCount(void) const {
    PHOOL_VIRTUAL_WARN("GL1ClockCount");
    return -9999;
  }
  virtual int get_stripCellID(const int module, const int chip) const {
    PHOOL_VIRTUAL_WARN("stripCellID");
    return -9999;
  }
  virtual unsigned char get_stripCellID(const int module, const int sensor,
                                        const int chip) const {
    PHOOL_VIRTUAL_WARN("stripCellID");
    return 0;
  }
  virtual int get_stripStatus(const int module) const {
    PHOOL_VIRTUAL_WARN("stripStatus");
    return -9999;
  }
  virtual int get_pixelStatus(const int module) const {
    PHOOL_VIRTUAL_WARN("pixelStatus");
    return -9999;
  }
  virtual int get_stripSubsysStatus(const int module) const {
    PHOOL_VIRTUAL_WARN("stripSubsysStatus");
    return -9999;
  }
  virtual int get_pixelSubsysStatus(const int module) const {
    PHOOL_VIRTUAL_WARN("pixelSubsysStatus");
    return -9999;
  }
  virtual int get_stripDcmStatus(const int module) const {
    PHOOL_VIRTUAL_WARN("stripDcmStatus");
    return -9999;
  }
  virtual int get_pixelDcmStatus(const int module) const {
    PHOOL_VIRTUAL_WARN("pixelDcmStatus");
    return -9999;
  }
  virtual int get_stripBeamClock(const int module) const {
    PHOOL_VIRTUAL_WARN("stripBeamClock");
    return -9999;
  }
  virtual int get_pixelBeamClock(const int module) const {
    PHOOL_VIRTUAL_WARN("pixelBeamClock");
    return -9999;
  }
  virtual int get_stripEventCount(const int module) const {
    PHOOL_VIRTUAL_WARN("stripEventCount");
    return -9999;
  }
  virtual int get_pixelEventCount(const int module) const {
    PHOOL_VIRTUAL_WARN("pixelEventCount");
    return -9999;
  }
  virtual unsigned char get_stripNRawhits(const int module) const {
    PHOOL_VIRTUAL_WARN("stripRawInfo");
    return 0;
  }
  virtual unsigned char get_stripNClusters(const int module) const {
    PHOOL_VIRTUAL_WARN("stripNClusters");
    return 0;
  }
  virtual unsigned char get_pixelNRawhits(const int ladder,
                                          const int sensor) const {
    PHOOL_VIRTUAL_WARN("pixelRawInfo");
    return 0;
  }
  virtual unsigned char get_pixelNClusters(const int ladder,
                                           const int sensor) const {
    PHOOL_VIRTUAL_WARN("pixelNClusters");
    return 0;
  }

  // Methods
  // """""""
  virtual SvxEventInfo* Clone() {
    PHOOL_VIRTUAL_WARN("Clone()");
    return 0;
  }
  TObject* Clone(const char* = "") const {
    PHOOL_VIRTUAL_WARNING;
    return 0;
  }
  virtual void Copy(SvxEventInfo* info) { PHOOL_VIRTUAL_WARN("Copy()"); }
  virtual void Copy(TObject& object) const { PHOOL_VIRTUAL_WARNING; }

  ClassDef(SvxEventInfo, 1);
};
#endif
