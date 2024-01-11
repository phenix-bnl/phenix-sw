#ifndef __MUTRGHIT_V2__
#define __MUTRGHIT_V2__

#include "MutrgPar.hh"
#include "MutrgHit.hh"

class MutrgHit_v2 : public MutrgHit{
public:
  MutrgHit_v2();
  virtual ~MutrgHit_v2(){;}
  MutrgHit* Create(void){return new MutrgHit_v2();}
  void Reset(void);
  void Clear(Option_t* ="");

  void SetLocation(int arm,int station,int octant,int halfoctant,
		   int gap,int cathode,int strip);
  void SetLocation(int arm,int station,int octant,int halfoctant,
		   int strip);
  void SetKey(unsigned int key){location=key;}
  void SetHitClock(unsigned short hit){hit_clock=hit;}
  void SetHitClock(unsigned short clk,bool hit);
  void SetState(unsigned short flag){state=flag;}
  void SetDeadFlag(bool flag);
  void SetMaskFlag(bool flag);
  int SetMutHitValue(int gap,float q,float qerr,float ptime);
  void SetMutMaxCharge(float q);

  void GetLocation(int &arm,int &station,int &octant,int &halfoctant,
		   int &gap,int &cathode,int &strip) const;
  void GetLocation(int &arm,int &station,int &octant,int &halfoctant,
		   int &strip) const;
  unsigned int GetKey(void) const{return location;}
  unsigned short GetHitClock(void) const{return hit_clock;}
  bool GetHitClock(unsigned short clk) const;
  unsigned short GetState(void) const{return state;}
  bool GetDeadFlag(void) const;
  bool GetMaskFlag(void) const;
  int GetMutHitValue(int gap,float &q,float &qerr,float &ptime) const;
  float GetMutMaxCharge(void) const;

  void ExtendHitClock(int nclk); // 0x0c -> 0x1c for nclk=1
  void ShiftHitClock(int nclk);  // 0x0c -> 0x18 for nclk=1

  virtual void print(std::ostream &os=std::cout) const;

protected:
  //[17:arm,15-16:station,12-14:octant,11:halfoctant,
  // 9-10:gap,8:cathode,0-7:strip]
  // gap and cathode should be always 0
  unsigned int location;
  unsigned short hit_clock; // [0:0th clock,,,]
  unsigned short state; // [1:mask_flag,0:dead_flag]

  ClassDef(MutrgHit_v2,1)
};

#endif /* __MUTRGHIT_V2__ */
