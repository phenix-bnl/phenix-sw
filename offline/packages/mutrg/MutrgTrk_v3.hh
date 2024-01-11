#ifndef __MUTRGTRK_V3__
#define __MUTRGTRK_V3__

#include "MutrgTrk.hh"

#include "MutrgPar.hh"

class MutrgTrk_v3 : public MutrgTrk{
public:
  MutrgTrk_v3(void);
  virtual ~MutrgTrk_v3(void);
  MutrgTrk* Create(void){return new MutrgTrk_v3();}
  void Reset(void);

  void SetIndex(unsigned short value){index=value;}
  void ClearMuTrIndex(void);
  void ClearMuTrUid(void);
  void AddMuTrIndex(unsigned short value);
  void AddMuTrUid(unsigned int value);
  int SetHit(int station,unsigned int hit_loc);
  void SetHitClock(unsigned short hit){hit_clock=hit;}
  void SetHitClock(unsigned short clk,bool hit);

  unsigned short GetIndex(void) const {return index;}
  int GetMuTrNIndex(void) const;
  unsigned short GetMuTrIndex(int num) const;
  int GetMuTrNUid(void) const {return nuid_mutr;}
  unsigned int GetMuTrUid(int num) const;
  unsigned int GetHit(int station) const;
  unsigned short GetHitClock(void) const {return hit_clock;}
  bool GetHitClock(unsigned short clk) const;

  void print(std::ostream &os=std::cout) const;

protected:
  int MAX_UID_MUTR; //! not stored in DST

  unsigned short index;
  int nuid_mutr;
  unsigned int *uid_mutr; //[nuid_mutr]
  unsigned int hit_location[MutrgPar::NSTATION];
  unsigned short hit_clock; // [0:0th clock,,,]

  ClassDef(MutrgTrk_v3,1)
};

#endif /* __MUTRGTRK_V3__ */
