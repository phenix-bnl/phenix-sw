#ifndef __MUTRGTRK_V1__
#define __MUTRGTRK_V1__

#include "MutrgTrk.hh"

#include "MutrgPar.hh"

class MutrgTrk_v1 : public MutrgTrk{
public:
  MutrgTrk_v1(void);
  virtual ~MutrgTrk_v1(void);
  MutrgTrk* Create(void){return new MutrgTrk_v1();}
  void Reset(void);

  void SetIndex(unsigned short value){index=value;}
  void ClearMuTrIndex(void);
  void ClearMuTrUid(void);
  void AddMuTrIndex(unsigned short value);
  void AddMuTrUid(unsigned int value);
  int SetHit(int station,unsigned int hit_loc);
  void SetHitClock(unsigned short hit);
  void SetHitClock(unsigned short clk,bool hit);

  unsigned short GetIndex(void) const {return index;}
  int GetMuTrNIndex(void) const {return nindex_mutr;}
  unsigned short GetMuTrIndex(int num) const;
  int GetMuTrNUid(void) const;
  unsigned int GetMuTrUid(int num) const;
  unsigned int GetHit(int station) const;
  unsigned short GetHitClock(void) const;
  bool GetHitClock(unsigned short clk) const;

  void print(std::ostream &os=std::cout) const;

protected:
  int MAX_INDEX_MUTR; //! not stored in DST

  unsigned short index;
  int nindex_mutr;
  UShort_t *index_mutr; //[nindex_mutr]
  unsigned int hit_location[MutrgPar::NSTATION];

  ClassDef(MutrgTrk_v1,1)
};

#endif /* __MUTRGTRK_V1__ */
