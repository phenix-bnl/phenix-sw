#ifndef __VTXOUTV5_H__
#define __VTXOUTV5_H__

#include <iostream>
#include "VtxReturncodes.h"
#include "VtxOutv4.h"

class PHPoint;

class VtxOutv5: public VtxOutv4
{
public:
  VtxOutv5();
  virtual ~VtxOutv5() {}

  void Reset();
  void identify(std::ostream& os = std::cout) const;
  VtxOutv5& operator=(const VtxOut &source);

  int set_CglVtx(const float vtx, const float vtxerr);

  PHPoint get_Vertex() const;
  PHPoint get_VertexError() const;

  float get_ZVertex() const;
  float get_ZVertexError() const;

  float get_CglVertex() const {return CglVtx;}
  float get_CglVertexError() const {return CglVtxErr;}

  int use_Vertex(const char *subsystem);

  bool isCglVtx() const;

  const char *which_Vtx() const;

protected:
  int ChooseVtx() const;  // here we determine the vertex of choice

  float CglVtx;
  float CglVtxErr;

  ClassDef(VtxOutv5,1)
};

#endif
