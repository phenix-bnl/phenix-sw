#ifndef __BBCMULTIPLEVTXCLUSTER__
#define __BBCMULTIPLEVTXCLUSTER__

/*
  BbcMultipleVtxCluster.hh
  
  Created: 2011/11/16
  Last Update: 2012/04/16
  Author: Hideyuki Oide
  
  Description:
  Data container of timing cluster of BBC.

 */

#include <PHObject.h>

class BbcMultipleVtxCluster : public PHObject {
private:
  
public:
  BbcMultipleVtxCluster();
  virtual ~BbcMultipleVtxCluster() {};
  virtual inline int get_order() { return -1; }
  virtual inline int get_size() { return -1; }
  virtual inline float get_tof() { return -9999.; }
  virtual inline void set_order(const int order) {}
  virtual inline void set_size(const int size) {}
  virtual inline void set_tof(const float tof) {}
  virtual inline void print() {};
  
  ClassDef(BbcMultipleVtxCluster, 1)
};

#endif /* __BBCMULTIPLEVTXCLUSTER__ */
