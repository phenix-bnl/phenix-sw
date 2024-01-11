#ifndef __VTXSNGLVTXV1_H
#define __VTXSNGLVTXV1_H

#include "TString.h"
#include "VtxSnglVtx.h"

class VtxSnglVtxv1 : public VtxSnglVtx
{
 public:
  VtxSnglVtxv1(); // default ctor to make root happy
  VtxSnglVtxv1(const VtxSnglVtxv1&);
  VtxSnglVtxv1(const char *name, const float *vertex, const float *vertexerr, const short int subsystem);
  virtual ~VtxSnglVtxv1() {}

  void Vtx(const short index, const float val) {vtx[index] = val;}
  float Vtx(const short index) const {return vtx[index];}
  void VtxErr(const short index, const float val) {vtxerr[index] = val;}
  float VtxErr(const short index) const {return vtxerr[index];}
  void Name(const char *name) {VtxName = name;}
  const char *Name() const {return VtxName.Data();}
  void SubSystem(const short isys) {Subsystem = isys;}
  short int SubSystem() const {return Subsystem;}
  

 protected:
  short int Subsystem;
  float vtx[3];
  float vtxerr[3];
  TString VtxName;

  ClassDef(VtxSnglVtxv1,1)
};

#endif /* __VTXSNGLVTXV1_H */
