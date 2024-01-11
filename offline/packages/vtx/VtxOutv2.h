#ifndef __VTXOUTV2_h
#define __VTXOUTV2_h

#include <iostream>
#include "PHPoint.h"
#include "VtxReturncodes.h"
#include "VtxOutv1.h"

class VtxOutv2: public VtxOutv1
{

 public:

  VtxOutv2();
  virtual ~VtxOutv2() {}

  void Reset();
  void identify(std::ostream& os = std::cout) const;

  int set_MvdVtx(const PHPoint *vtx, const PHPoint *vtxerr, const float conflevel);
  int set_MvdVtx(const PHPoint *vtx, const PHPoint *vtxerr) {
    return set_MvdVtx(vtx, vtxerr, VTX_INVALID_FLOAT);
  }

  PHPoint get_Vertex() const;
  PHPoint get_VertexError() const;
  float get_ZVertex() const;
  float get_ZVertexError() const;

  int set_DefaultVtx(const PHPoint *vtx, const PHPoint *vtxerr);

  int  isValid() const;
  bool isMvdVtx() const;

  int use_Vertex(const PHPoint *vtx, const PHPoint *vtxerr);
  int use_Vertex(const char *subsystem);
  
  const char *which_Vtx() const;

 protected:
  int ChooseVtx() const;  // here we determine the vertex of choice
  float MvdConfLevel;
  float DefaultVtx[3];
  float DefaultVtxErr[3];

  ClassDef(VtxOutv2,1)

};

#endif
