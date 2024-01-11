#ifndef __TFvtxTrigEmulInfo_HH_
#define __TFvtxTrigEmulInfo_HH_

/*!
  \file    TFvtxTrigEmulInfo.h
  \brief   PHObject holding FVTX trigger emulator output information
  \author  D. McGlinchey
  \version $Revision: 1.2 $
  \date    $Date: 2016/04/26 19:59:27 $
*/

#include <phool.h>
#include <PHObject.h>
#include <iostream>


class TFvtxTrigEmulInfo : public PHObject
{

public:

  /*! Constructor */
  TFvtxTrigEmulInfo(TFvtxTrigEmulInfo* info = NULL) {}

  /*! Destructor */
  virtual ~TFvtxTrigEmulInfo() {}

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void identify(std::ostream &os = std::cout) const {
    os << "Identify yourself: virtual TFvtxTrigEmulInfo object" << std::endl;
  }

  /// Clear Event
  virtual void Reset() { PHOOL_VIRTUAL_WARN("Reset"); }

  /// isValid returns non zero if object contains vailid data
  virtual int isValid() const {
    PHOOL_VIRTUAL_WARN("isValid");
    return -9999;
  }


  //! @name Setters
  //@{

  virtual void set_trig(bool trig) {
    PHOOL_VIRTUAL_WARN("set_trig()");
  }

  virtual void set_trigArm(bool trig,
                           int arm) {
    PHOOL_VIRTUAL_WARN("set_trigArm()");
  }

  virtual void set_trigCage(bool trig,
                            int arm,
                            int cage) {
    PHOOL_VIRTUAL_WARN("set_trigCage()");
  }

  virtual void set_trigFEM(bool trig,
                           int arm,
                           int cage,
                           int fem) {
    PHOOL_VIRTUAL_WARN("set_trigFEM()");
  }

  virtual void set_trigSector(bool trig,
                              int arm,
                              int cage,
                              int sector) {
    PHOOL_VIRTUAL_WARN("set_trigSector()");
  }

  virtual void set_trigStation(bool trig,
                               int arm,
                               int cage,
                               int sector,
                               int station) {
    PHOOL_VIRTUAL_WARN("set_trigStation()");
  }

  //@}

  //! @name Get trigger decisions
  //@{

  virtual bool did_trigFire() {
    PHOOL_VIRTUAL_WARN("did_trigFire()");
    return false;
  }

  virtual bool did_trigFireArm(int arm) {
    PHOOL_VIRTUAL_WARN("did_trigFireArm()");
    return false;
  }

  virtual bool did_trigFireCage(int arm,
                                int cage) {
    PHOOL_VIRTUAL_WARN("did_trigFireCage()");
    return false;
  }

  virtual bool did_trigFireFEM(int arm,
                               int cage,
                               int fem) {
    PHOOL_VIRTUAL_WARN("did_trigFireFEM()");
    return false;
  }

  virtual bool did_trigFireSector(int arm,
                                  int cage,
                                  int sector) {
    PHOOL_VIRTUAL_WARN("did_trigFireSector()");
    return false;
  }

  virtual bool did_trigFireStation(int arm,
                                   int cage,
                                   int sector,
                                   int station) {
    PHOOL_VIRTUAL_WARN("did_trigFireStation()");
    return false;
  }


  //@}


  virtual TFvtxTrigEmulInfo* Clone() {
    PHOOL_VIRTUAL_WARN("Clone()");
    return 0;
  }
  TObject* Clone(const char* = "") const {
    PHOOL_VIRTUAL_WARNING;
    return 0;
  }
  virtual void Copy(TFvtxTrigEmulInfo* info) { PHOOL_VIRTUAL_WARN("Copy()"); }
  virtual void Copy(TObject& object) const { PHOOL_VIRTUAL_WARNING; }

  ClassDef(TFvtxTrigEmulInfo, 1);
};
#endif
