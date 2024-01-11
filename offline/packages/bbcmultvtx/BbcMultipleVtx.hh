#ifndef __BBCMULTIPLEVTX__
#define __BBCMULTIPLEVTX__

#include <PHObject.h>

class BbcMultipleVtxList;

class BbcMultipleVtx : public PHObject {
private:
  
public:
  BbcMultipleVtx();
  virtual ~BbcMultipleVtx();
  virtual void Reset();
  virtual BbcMultipleVtxList* get_vtxlist(const int tune) const;
  virtual const int get_size() const;
  virtual const float get_bbcout_zvtx() const;
  virtual const float get_bbcout_t0() const;
  virtual void add_vtxlist(BbcMultipleVtxList *vtxlist);
  virtual void set_bbcout_zvtx(const float zvtx);
  virtual void set_bbcout_t0(const float t0);
  virtual void print() const;
  
  ClassDef(BbcMultipleVtx, 1)
};

#endif /* __BBCMULTIPLEVTX__ */
