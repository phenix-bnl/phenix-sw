#ifndef __VTXSNGLVTX_H
#define __VTXSNGLVTX_H

#include <string>
#include "PHObject.h"

class VtxSnglVtx : public PHObject
{
 public:

  virtual ~VtxSnglVtx() {}
  virtual VtxSnglVtx *clone() const;
  virtual VtxSnglVtx& operator=(const VtxSnglVtx& source);
  virtual void Vtx(const short index, const float vtx) {return;}
  virtual float Vtx(const short index) const;
  virtual void VtxErr(const short index, const float vtx) {return;}
  virtual float VtxErr(const short index) const;
  virtual void Name(const char *name) {return;}
  virtual const char *Name() const {return "DUMMY";}
  virtual void SubSystem(const short isys) {return;}
  virtual short int SubSystem() const {return -9999;}

  ClassDef(VtxSnglVtx,1)
};

#endif /* __VTXSNGLVTX_H */
