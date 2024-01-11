#ifndef __BBCMULTIPLEVTXLIST__
#define __BBCMULTIPLEVTXLIST__

/*
  BbcMultipleVtxList.hh
  
  Created: 2011/11/16
  Last Update: 2012/03/30
  Author: Hideyuki Oide
  
  Description:
  Data container of clustered BBC vertex reconstruction for a particular binsize.

 */

#include <PHObject.h>
#include <Bbc.hh>

class BbcMultipleVtxCluster;
class BbcMultipleVtxPoint;

class BbcMultipleVtxList : public PHObject {
public:
  BbcMultipleVtxList();
  virtual ~BbcMultipleVtxList();
  virtual void Reset();
  virtual void print();
  
  virtual void set_binsize(const float size);
  virtual const float get_binsize() const;
  
  virtual void add_cluster(const int arm, BbcMultipleVtxCluster *cluster);
  virtual void add_vtx_point(BbcMultipleVtxPoint *vtxpoint);
  virtual void add_cluster(const int arm, const int order, const int size, const float tof);
  virtual void add_vtx_point(const int order_south, const int order_north, const float vtxz, const float t0);
  virtual const int get_cluster_number(const int arm) const;
  
  virtual BbcMultipleVtxCluster* get_cluster(const int arm, const  int order) const;
  virtual BbcMultipleVtxPoint* get_vertex_point(const  int n) const;
  
  virtual const int get_cluster_size(const int arm, const  int order) const;
  virtual const float get_cluster_tof(const int arm, const  int order) const;
  
  virtual const int get_vertex_number() const;
  virtual const float get_vertex_z(const  int n) const;
  virtual const float get_vertex_t0(const  int n) const;
  
  ClassDef(BbcMultipleVtxList, 1)
};

#endif /* __BBCMULTIPLEVTXLIST__ */
