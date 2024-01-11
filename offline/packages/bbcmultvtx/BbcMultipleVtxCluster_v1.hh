#ifndef __BBCMULTIPLEVTXCLUSTER_V1__
#define __BBCMULTIPLEVTXCLUSTER_V1__

/*
  BbcMultipleVtxCluster_v1.hh
  
  Created: 2012/04/16
  Last Update: 2012/04/16
  Author: Hideyuki Oide
  
  Description:
  Data container of timing cluster of BBC.

 */

#include "BbcMultipleVtxCluster.hh"

class BbcMultipleVtxCluster_v1 : public BbcMultipleVtxCluster {
private:
  int fOrder;
  int fSize;
  float fTof;
  
public:
  BbcMultipleVtxCluster_v1();
  virtual ~BbcMultipleVtxCluster_v1() {};
  inline int get_order() { return fOrder; }
  inline int get_size() { return fSize; }
  inline float get_tof() { return fTof; }
  inline void set_order(const int order) { fOrder = order; };
  inline void set_size(const int size) { fSize = size; };
  inline void set_tof(const float tof) { fTof = tof; };
  void print();
  
  ClassDef(BbcMultipleVtxCluster_v1, 1)
};

#endif /* __BBCMULTIPLEVTXCLUSTER_V1__ */
