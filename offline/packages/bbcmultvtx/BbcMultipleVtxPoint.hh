#ifndef __BBCMULTIPLEVTXPOINT__
#define __BBCMULTIPLEVTXPOINT__

/*
  BbcMultipleVtxPoint.hh
  
  Created: 2011/11/16
  Last Update: 2011/04/16
  Author: Hideyuki Oide
  
  Description:
  Data container of vertex point for Clustering BBC Reconstruction.

 */

#include <PHObject.h>

class BbcMultipleVtxPoint : public PHObject {
private:
public:
  BbcMultipleVtxPoint();
  virtual ~BbcMultipleVtxPoint() {};
  virtual inline void reset() {}
  virtual inline void print() {}
  
  virtual inline const float get_vtxz() const { return -9999.; }
  virtual inline const float get_t0() const { return -9999.; }
  virtual inline const int get_cluster_order(const int arm) const { return -1; }
  virtual inline void set_vtxz(const float vtxz) {}
  virtual inline void set_t0(const float t0) {}
  virtual inline void set_cluster_order(const int arm, const int order) {}
  
  ClassDef(BbcMultipleVtxPoint, 1)
    
};

#endif /* __BBCMULTIPLEVTXPOINT__ */
