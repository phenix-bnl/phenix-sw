#ifndef __VTXOUTV7_H__
#define __VTXOUTV7_H__

#include "VtxOutv6.h"

class VtxOutv7 : public VtxOutv6
{
 public:
  VtxOutv7();
  VtxOutv7(const VtxOutv7&);
  virtual ~VtxOutv7();
  VtxOutv7* clone() const { return new VtxOutv7(*this); }
  int DeepCopy(const VtxOut *vtxout);
  void identify(std::ostream &os = std::cout) const;
  
  //! add vertex with a given name and with a given order
  virtual void AddVtx(const char *name, const float *vertex, const float *vertexerr, const short int order);
  virtual void AddVtx(const char *name, const float *vertex, const float *vertexerr, const VTX::Order order);

  protected:
  void copyTo(VtxOutv7& dest) const;
  ClassDef(VtxOutv7,1)

};

#endif /*__VTXOUTV7_H__*/
