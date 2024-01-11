#include "BbcMultipleVtxCluster_v1.hh"

ClassImp(BbcMultipleVtxCluster_v1)

BbcMultipleVtxCluster_v1::BbcMultipleVtxCluster_v1()
  : fOrder(0), fSize(0), fTof(-9999.0)
{}

void BbcMultipleVtxCluster_v1::print() {
  printf("    Cluster [%2d]:   Size = %2d,   TOF = %6.2f ns\n",
	 fOrder, fSize, fTof);
}
