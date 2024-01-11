#ifndef __BBCMULTIPLEVTX_V2__
#define __BBCMULTIPLEVTX_V2__

#include "BbcMultipleVtx.hh"

class BbcMultipleVtxList;
class TClonesArray;

class BbcMultipleVtx_v2 : public BbcMultipleVtx {
private:
  TClonesArray *vtxlists;
  float fBbcOut_zvtx;
  float fBbcOut_t0;
  
  inline void add_vtxlist(BbcMultipleVtxList *vtxlist){}
public:
  BbcMultipleVtx_v2(const Int_t kNtimebin = 0);
  virtual ~BbcMultipleVtx_v2();
  void Reset();
  BbcMultipleVtxList* get_vtxlist(const int tune) const;
  const int get_size() const;
  const float get_bbcout_zvtx() const;
  const float get_bbcout_t0() const;
  void set_bbcout_zvtx(const float zvtx);
  void set_bbcout_t0(const float t0);
  void print() const;
  
  ClassDef(BbcMultipleVtx_v2, 1)
};

#endif /* __BBCMULTIPLEVTX_V2__ */
