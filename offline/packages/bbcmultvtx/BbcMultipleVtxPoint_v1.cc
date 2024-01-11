#include "Bbc.hh"
#include "BbcMultipleVtxPoint_v1.hh"

ClassImp(BbcMultipleVtxPoint_v1)

BbcMultipleVtxPoint_v1::BbcMultipleVtxPoint_v1()
  : fVtxZ(-9999.0)
  , fT0(-9999.0)
{
  cluster_order[Bbc::South] = cluster_order[Bbc::North] = -1;
}

void BbcMultipleVtxPoint_v1::reset() {
  fVtxZ = -9999.0;
  fT0 = -9999.0;
  cluster_order[Bbc::South] = cluster_order[Bbc::North] = -1;
  
  return;
}

void BbcMultipleVtxPoint_v1::print() {
  printf("    Vtx North [%2d] - South [%2d]:   VtxZ = %6.2f cm,   T0 = %6.2f ns\n",
	 cluster_order[Bbc::North], cluster_order[Bbc::South], fVtxZ, fT0 );
}
