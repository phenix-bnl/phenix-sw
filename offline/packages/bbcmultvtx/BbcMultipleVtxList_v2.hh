#ifndef __BBCMULTIPLEVTXLIST_v2__
#define __BBCMULTIPLEVTXLIST_v2__

/*
  BbcMultipleVtxList_v2.hh
  
  Created: 2011/03/30
  Last Update: 2011/03/30
  Author: Hideyuki Oide
  
  Description:
  Data container of clustered BBC vertex reconstruction for a particular binsize.

 */

#include <BbcMultipleVtxList.hh>
#include <Bbc.hh>

class BbcMultipleVtxCluster;
class BbcMultipleVtxPoint;

class TClonesArray;

class BbcMultipleVtxList_v2 : public BbcMultipleVtxList {
private:
  float binsize;
  
  TClonesArray *clusters_north;
  TClonesArray *clusters_south;
  TClonesArray *vertex_points;
  
  inline void add_cluster(const int arm, BbcMultipleVtxCluster *cluster){}
  inline void add_vtx_point(BbcMultipleVtxPoint *vtxpoint){}
public:
  BbcMultipleVtxList_v2();
  virtual ~BbcMultipleVtxList_v2();
  void Reset();
  void print();
  
  inline void set_binsize(const float size) { binsize = size; }
  inline const float get_binsize() const { return binsize; }
  
  void add_cluster(const int arm, const int order, const int size, const float tof);
  void add_vtx_point(const int order_south, const int order_north, const float vtxz, const float t0);
  const int get_cluster_number(const int arm) const;
  
  BbcMultipleVtxCluster* get_cluster(const int arm, const  int order) const;
  BbcMultipleVtxPoint* get_vertex_point(const  int n) const;
  
  const int get_cluster_size(const int arm, const  int order) const;
  const float get_cluster_tof(const int arm, const  int order) const;
  
  const int get_vertex_number() const;
  const float get_vertex_z(const  int n) const;
  const float get_vertex_t0(const  int n) const;
  
  ClassDef(BbcMultipleVtxList_v2, 1)
};

#endif /* __BBCMULTIPLEVTXLIST_v2__ */
