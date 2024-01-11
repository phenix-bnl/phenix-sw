#ifndef __VTXOUTV3_h
#define __VTXOUTV3_h

#include <iostream>
#include "VtxReturncodes.h"
#include "VtxOut.h"

class PHPoint;

class VtxOutv3: public VtxOut
{

 public:

  VtxOutv3();
  virtual ~VtxOutv3() {}

  void Reset();
  void identify(std::ostream& os = std::cout) const;

  int set_BbcVtx(const float vtx, const float vtxerr);
  int set_NtcVtx(const float vtx, const float vtxerr);
  int set_ZdcVtx(const float vtx, const float vtxerr);

  int set_DefaultVtx(const PHPoint *vtx, const PHPoint *vtxerr);

  PHPoint get_Vertex() const;
  PHPoint get_VertexError() const;

  float get_ZVertex() const;
  float get_ZVertexError() const;

  float get_BbcVertex() const {return BbcVtx;}
  float get_BbcVertexError() const {return BbcVtxErr;}

  float get_NtcVertex() const {return NtcVtx;}
  float get_NtcVertexError() const {return NtcVtxErr;}

  float get_ZdcVertex() const {return ZdcVtx;}
  float get_ZdcVertexError() const {return ZdcVtxErr;}

  int get_VtxList() const {return VtxList;}
  int use_Vertex(const PHPoint *vtx, const PHPoint *vtxerr);
  int use_Vertex(const char *subsystem);

  bool isBbcVtx() const;
  bool isNtcVtx() const;
  bool isZdcVtx() const;

  int isValid() const {return VtxList;}
  const char *which_Vtx() const;

 protected:
  int ChooseVtx() const;  // here we determine the vertex of choice

  int VtxList;            // store bits of subsystems with vtx 
  float BbcVtx;
  float BbcVtxErr;
  float NtcVtx;
  float NtcVtxErr;
  float ZdcVtx;
  float ZdcVtxErr;
  float DefaultVtx[3];
  float DefaultVtxErr[3];

  ClassDef(VtxOutv3,1)

};

#endif
