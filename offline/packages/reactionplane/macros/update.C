#include <../RpConst.h>

void update(const int run=373139)
{

  gSystem->Load("libreactionplane.so");

  ReactionPlaneCalibv1* rpcalib = new ReactionPlaneCalibv1();

  // Fetch parameters
  //const char* filename = Form("par/reactionPlane.table.run%d",run);
  //rpcalib->SetRunNumber(run);
  //rpcalib->Fetch(filename);
  //
  cout<<RP::ID_FVT<<endl;

  // Update parameters in DB
  rpcalib->Fetch("/direct/phenix+plhf2/hiro_n/taxi/Run12CuAu200MinBias/2932/data/macro/updateflatDB/rpcalib_flat_373139.dat");
  //rpcalib->Write("tmp_373139.dat");
  
  const int beginrun = run;
  const int endrun   = run;

  rpcalib->Update(beginrun, endrun);
  //rpcalib->Update(RP::ID_SMD, beginrun, endrun);
}
