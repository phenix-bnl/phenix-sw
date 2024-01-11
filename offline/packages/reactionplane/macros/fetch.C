#include <../RpConst.h>

void fetch(const int run=373139)
{
/*
  gSystem->Load("libreactionplane.so");

  ReactionPlaneCalib* rpcalib = ReactionPlaneCalib::instance();

  // Fetch parameters
  rpcalib->Fetch(run);

  // Write
  const char* filename = Form("reactionPlane.table.run%d",run);
  rpcalib->Write(filename);
*/

  gSystem->Load("libreactionplane.so");

  ReactionPlaneCalibv1* rpcalib = new ReactionPlaneCalibv1();
//  rpcalib->Fetch("/direct/phenix+plhf2/hiro_n/taxi/Run12CuAu200MinBias/2932/data/macro/updateflatDB/rpcalib_flat_373139.dat");
//  rpcalib->test();
  
//  int run = 373139;
  // Write
  rpcalib->Fetch(run);
  //rpcalib->Fetch(RP::ID_SMD, run);
  //rpcalib->Fetch(RP::ID_FVT, run);

  const char* filename = Form("./rpcalib_flat_%d_fetch.dat",run);
  rpcalib->Write(filename);
}
