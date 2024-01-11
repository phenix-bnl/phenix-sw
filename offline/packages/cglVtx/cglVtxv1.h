#ifndef __CGLVTXV1_H__
#define __CGLVTXV1_H__

#include "cglVtx.h"

class cglVtxv1: public cglVtx 
{
public:
  cglVtxv1() {_v = false;}
  ~cglVtxv1() {}
  PHBoolean event(PHCompositeNode *r);
  void reset() {_v = false;}
  int isValid() const { return _v; }
  double Zvertex() {return _z;}
  double ZvertexError() {return _zerr;}

protected:
  bool _v;
  double _z;
  double _zerr;
};

#endif /* __CGLVTXV1_H__ */
