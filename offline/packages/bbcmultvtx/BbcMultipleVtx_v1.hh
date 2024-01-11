#ifndef __BBCMULTIPLEVTX_V1__
#define __BBCMULTIPLEVTX_V1__

#include "BbcMultipleVtx.hh"

class BbcMultipleVtxList;
class TObjArray;

class BbcMultipleVtx_v1 : public BbcMultipleVtx {
private:
  TObjArray *vtxlists;
  float fBbcOut_zvtx;
  float fBbcOut_t0;
  
public:
  BbcMultipleVtx_v1();
  virtual ~BbcMultipleVtx_v1();
  void Reset();
  BbcMultipleVtxList* get_vtxlist(const int tune) const;
  const int get_size() const;
  const float get_bbcout_zvtx() const;
  const float get_bbcout_t0() const;
  void add_vtxlist(BbcMultipleVtxList *vtxlist);
  void set_bbcout_zvtx(const float zvtx);
  void set_bbcout_t0(const float t0);
  void print() const;
  
  ClassDef(BbcMultipleVtx_v1, 1)
};

#endif /* __BBCMULTIPLEVTX_V1__ */
