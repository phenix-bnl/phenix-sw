#ifndef __MUTRGTRK__
#define __MUTRGTRK__

#include "TObject.h"
#include <phool.h>

#include <iostream>
#include <string>

class MutrgTrk : public TObject{
public:
  MutrgTrk() {}
  virtual ~MutrgTrk() {}
  virtual MutrgTrk* Create() { return new MutrgTrk(); }
  virtual void Reset() { PHOOL_VIRTUAL_WARNING; }

  virtual void SetIndex(unsigned short value) { PHOOL_VIRTUAL_WARNING; }
  virtual void ClearMuTrIndex() { PHOOL_VIRTUAL_WARNING; }
  virtual void ClearMuTrUid() { PHOOL_VIRTUAL_WARNING; }
  virtual void AddMuTrIndex(unsigned short value) { PHOOL_VIRTUAL_WARNING; }
  virtual void AddMuTrUid(unsigned int value) { PHOOL_VIRTUAL_WARNING; }
  virtual int SetHit(int station, unsigned int hit_loc) {
    PHOOL_VIRTUAL_WARNING;
    return -1;
  }
  virtual void SetHitClock(unsigned short hit) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetHitClock(unsigned short clk, bool hit) {
    PHOOL_VIRTUAL_WARNING;
  }

  virtual unsigned short GetIndex() const {
    PHOOL_VIRTUAL_WARNING;
    return 0;
  }
  virtual int GetMuTrNIndex() const {
    PHOOL_VIRTUAL_WARNING;
    return -1;
  }
  virtual unsigned short GetMuTrIndex(int num) const {
    PHOOL_VIRTUAL_WARNING;
    return 0;
  }
  virtual int GetMuTrNUid() const {
    PHOOL_VIRTUAL_WARNING;
    return -1;
  }
  virtual unsigned int GetMuTrUid(int num) const {
    PHOOL_VIRTUAL_WARNING;
    return 0;
  }
  virtual unsigned int GetHit(int station) const {
    PHOOL_VIRTUAL_WARNING;
    return 0;
  }
  virtual unsigned short GetHitClock() const {
    PHOOL_VIRTUAL_WARNING;
    return 0;
  }
  virtual bool GetHitClock(unsigned short clk) const {
    PHOOL_VIRTUAL_WARNING;
    return 0;
  }

  virtual bool CheckStation(int station) const;

  virtual void print(std::ostream &os=std::cout) const;

  ClassDef(MutrgTrk,1)
};

#endif /* __MUTRGTRK__ */
