#ifndef __BBCMULTIPLEVTXPOINT_V1__
#define __BBCMULTIPLEVTXPOINT_V1__

/*
  BbcMultipleVtxPoint_v1.hh
  
  Created: 2012/04/16
  Last Update: 2012/04/16
  Author: Hideyuki Oide
  
  Description:
  Data container of vertex point for Clustering BBC Reconstruction.

 */

#include "BbcMultipleVtxPoint.hh"

class BbcMultipleVtxPoint_v1 : public BbcMultipleVtxPoint {
private:
  float fVtxZ;
  float fT0;
  int cluster_order[2];
  
public:
  BbcMultipleVtxPoint_v1();
  virtual ~BbcMultipleVtxPoint_v1() {};
  void reset();
  void print();
  
  inline const float get_vtxz() const { return fVtxZ; }
  inline const float get_t0() const { return fT0; }
  inline const int get_cluster_order(const int arm) const { return cluster_order[arm]; }
  inline void set_vtxz(const float vtxz) { fVtxZ = vtxz; }
  inline void set_t0(const float t0) { fT0 = t0; }
  inline void set_cluster_order(const int arm, const int order) { cluster_order[arm] = order; }
  
  ClassDef(BbcMultipleVtxPoint_v1, 1)
    
};

#endif /* __BBCMULTIPLEVTXPOINT_V1__ */
