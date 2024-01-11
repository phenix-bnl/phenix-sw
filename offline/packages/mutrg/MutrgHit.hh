#ifndef __MUTRGHIT__
#define __MUTRGHIT__

#include <phool.h>
#include "TObject.h"

#include <iostream>

class MutrgHit : public TObject{
public:
  MutrgHit(){;}
  virtual ~MutrgHit(){;}
  virtual MutrgHit *Create() { return new MutrgHit(); }
  virtual void Reset() { PHOOL_VIRTUAL_WARNING; }
  virtual void Clear(Option_t* ="") { PHOOL_VIRTUAL_WARNING; }

  virtual void Set(const MutrgHit *hit_org);
  virtual void SetLocation(int arm, int station, int octant, int halfoctant,
                           int gap, int cathode, int strip) {
    PHOOL_VIRTUAL_WARNING;
  }
  virtual void SetLocation(int arm, int station, int octant, int halfoctant,
                           int strip) {
    PHOOL_VIRTUAL_WARNING;
  }
  virtual void SetKey(unsigned int key) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetHitClock(unsigned short hit) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetHitClock(unsigned short clk, bool hit) {
    PHOOL_VIRTUAL_WARNING;
  }
  virtual void SetState(unsigned short flag) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetDeadFlag(bool flag) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetMaskFlag(bool flag) { PHOOL_VIRTUAL_WARNING; }
  virtual int SetMutHitValue(int gap, float q, float qerr, float ptime) {
    PHOOL_VIRTUAL_WARNING;
    return -1;
  }
  virtual void SetMutMaxCharge(float q) { PHOOL_VIRTUAL_WARNING; }

  virtual void GetLocation(int &arm, int &station, int &octant, int &halfoctant,
                           int &gap, int &cathode, int &strip) const {
    PHOOL_VIRTUAL_WARNING;
  }
  virtual void GetLocation(int &arm, int &station, int &octant, int &halfoctant,
                           int &strip) const {
    PHOOL_VIRTUAL_WARNING;
  }
  virtual unsigned int GetKey() const {
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
  virtual unsigned short GetState() const {
    PHOOL_VIRTUAL_WARNING;
    return 0;
  }
  virtual bool GetDeadFlag() const {
    PHOOL_VIRTUAL_WARNING;
    return false;
  }
  virtual bool GetMaskFlag() const {
    PHOOL_VIRTUAL_WARNING;
    return false;
  }
  virtual int GetMutHitValue(int gap, float &q, float &qerr,
                             float &ptime) const {
    PHOOL_VIRTUAL_WARNING;
    return -1;
  }
  virtual float GetMutMaxCharge() const {PHOOL_VIRTUAL_WARNING; return -1;}

  virtual void ExtendHitClock(int nclk) {  // 0x0c -> 0x1c for nclk=1
    PHOOL_VIRTUAL_WARNING;
  }
  virtual void ShiftHitClock(int nclk) {  // 0x0c -> 0x18 for nclk=1
    PHOOL_VIRTUAL_WARNING;
  }

  static void Message(const char *msg,std::ostream &os=std::cout);

  virtual void print(std::ostream &os=std::cout) const;

  ClassDef(MutrgHit,1)
};

#endif /* __MUTRGHIT__ */
