#include "BbcMultipleVtxList.hh"
#include <cmath>

using namespace std;

ClassImp(BbcMultipleVtxList)

BbcMultipleVtxList::BbcMultipleVtxList(){}

BbcMultipleVtxList::~BbcMultipleVtxList() {}

void BbcMultipleVtxList::Reset() {
  return;
}

void BbcMultipleVtxList::print() {
  return;
}

void BbcMultipleVtxList::set_binsize(const float size) {
  return;
}

const float BbcMultipleVtxList::get_binsize() const {
  return NAN;
}
  
void BbcMultipleVtxList::add_cluster(const int arm, BbcMultipleVtxCluster *cluster) {
  cout << "BbcMultipleVtxList::add_cluster(const int arm, BbcMultipleVtxCluster *cluster) is obsolete" << endl;
}

void BbcMultipleVtxList::add_vtx_point(BbcMultipleVtxPoint *vtxpoint) {
  cout << "BbcMultipleVtxList::add_vtx_point(BbcMultipleVtxPoint *vtxpoint) is opsolete" << endl;
}

void BbcMultipleVtxList::add_cluster(const int arm, const int order, const int size, const float tof) {
  return;
}

void BbcMultipleVtxList::add_vtx_point(const int order_south, const int order_north, const float vtxz, const float t0) {
  return;
}

const int BbcMultipleVtxList::get_cluster_number(const int arm) const {
  return -1;
}

BbcMultipleVtxCluster* BbcMultipleVtxList::get_cluster(const int arm, const int order) const {
  return 0;
}

BbcMultipleVtxPoint* BbcMultipleVtxList::get_vertex_point(const int n) const {
  return 0;
}

const int BbcMultipleVtxList::get_cluster_size(const int arm, const int order) const {
  return -1;
}

const float BbcMultipleVtxList::get_cluster_tof(const int arm, const int order) const {
  return -9999.0;
}


const int BbcMultipleVtxList::get_vertex_number() const {
  return -1;
}


const float BbcMultipleVtxList::get_vertex_z(const int n) const {
  return -9999.0;
}


const float BbcMultipleVtxList::get_vertex_t0(const int n) const {
  return -9999.0;
}
