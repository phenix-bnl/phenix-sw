#ifndef __VTXOUTV4_H__
#define __VTXOUTV4_H__

#include <iostream>
#include "VtxReturncodes.h"
#include "VtxOut.h"

class PHPoint;

class VtxOutv4: public VtxOut
{

 public:

  VtxOutv4();
  virtual ~VtxOutv4() {}

  void Reset();
  void identify(std::ostream& os = std::cout) const;

  int set_BbcVtx(const float vtx, const float vtxerr);
  int set_NtcVtx(const float vtx, const float vtxerr);
  int set_MuonVtx(const float vtx, const float vtxerr);
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

  float get_MuonVertex() const {return MuonVtx;}
  float get_MuonVertexError() const {return MuonVtxErr;}

  float get_ZdcVertex() const {return ZdcVtx;}
  float get_ZdcVertexError() const {return ZdcVtxErr;}

  int get_VtxList() const {return VtxList;}
  int use_Vertex(const PHPoint *vtx, const PHPoint *vtxerr);
  int use_Vertex(const char *subsystem);

  bool isBbcVtx() const;
  bool isNtcVtx() const;
  bool isMuonVtx() const;
  bool isZdcVtx() const;

  int isValid() const;
  const char *which_Vtx() const;

 protected:
  int ChooseVtx() const;  // here we determine the vertex of choice

  int VtxList;            // store bits of subsystems with vtx 
  float BbcVtx;
  float BbcVtxErr;
  float NtcVtx;
  float NtcVtxErr;
  float MuonVtx;
  float MuonVtxErr;
  float ZdcVtx;
  float ZdcVtxErr;
  float DefaultVtx[3];
  float DefaultVtxErr[3];

  ClassDef(VtxOutv4,1)

};

#endif
