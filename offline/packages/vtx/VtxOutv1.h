#ifndef __VTXOUTV1_h
#define __VTXOUTV1_h

#include <iostream>
#include "PHPoint.h"
#include "VtxReturncodes.h"
#include "VtxOut.h"

class VtxOutv1: public VtxOut
{

 public:

  VtxOutv1();
  virtual ~VtxOutv1() {}

  void Reset();
  void identify(std::ostream& os = std::cout) const;

  int set_BbcVtx(const float vtx, const float vtxerr);
  int set_MvdVtx(const PHPoint *vtx, const PHPoint *vtxerr);
  int set_PadVtx(const float vtx, const float vtxerr);
  int set_ZdcVtx(const float vtx, const float vtxerr);

  PHPoint get_Vertex() const;
  PHPoint get_VertexError() const;
  float get_ZVertex() const;
  float get_ZVertexError() const;

  float get_BbcVertex() const {return BbcVtx;}
  float get_BbcVertexError() const {return BbcVtxErr;}

  PHPoint get_MvdVertex() const;
  PHPoint get_MvdVertexError() const;

  float get_PadVertex() const {return PadVtx;}
  float get_PadVertexError() const {return PadVtxErr;}

  float get_ZdcVertex() const {return ZdcVtx;}
  float get_ZdcVertexError() const {return ZdcVtxErr;}

  int get_VtxList() const {return VtxList;}

  bool isBbcVtx() const;
  bool isPadVtx() const;
  bool isZdcVtx() const;
  bool isMvdVtx() const;

  int isValid() const {return VtxList;}

 protected:
  int ChooseVtx() const;  // here we determine the vertex of choice

  int VtxList;            // store bits of subsystems with vtx 
  float BbcVtx;
  float BbcVtxErr;
  float MvdVtx[3];
  float MvdVtxErr[3];
  float PadVtx;
  float PadVtxErr;
  float ZdcVtx;
  float ZdcVtxErr;

  ClassDef(VtxOutv1,1)

};

#endif
