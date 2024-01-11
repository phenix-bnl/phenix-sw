#ifndef __VTXSNGLVTXV2_H
#define __VTXSNGLVTXV2_H

#include <string>
#include "VtxSnglVtx.h"

class VtxSnglVtxv2 : public VtxSnglVtx
{
 public:
  VtxSnglVtxv2(); // default ctor to make root happy
  VtxSnglVtxv2(const VtxSnglVtxv2 &source);
  VtxSnglVtxv2(const char *name, const float *vertex, const float *vertexerr, const short int subsystem);
  virtual ~VtxSnglVtxv2() {}

  VtxSnglVtxv2 *clone() const {return new VtxSnglVtxv2(*this);}
  void identify(std::ostream &out = std::cout) const;

  void Vtx(const short index, const float val) {vtx[index] = val;}
  float Vtx(const short index) const {return vtx[index];}
  void VtxErr(const short index, const float val) {vtxerr[index] = val;}
  float VtxErr(const short index) const {return vtxerr[index];}
  void Name(const char *name) {VtxName = name;}
  const char *Name() const {return VtxName.c_str();}
  void SubSystem(const short isys) {Subsystem = isys;}
  short int SubSystem() const {return Subsystem;}

 protected:
  short int Subsystem;
  float vtx[3];
  float vtxerr[3];
  std::string VtxName;

  ClassDef(VtxSnglVtxv2,1)
};

#endif /* __VTXSNGLVTXV2_H */
