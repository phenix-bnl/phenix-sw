//************************************************************
// Parameter set-up macro written by the Pyrite GUI for PHOOL 
//************************************************************

// This macro is valid for the new library dated Aug 8 at 06:53 am.

{

  // Setting dBbcGeo Parameters
  size_t nrc=1;
  dBbcGeo->SetRowCount(nrc);
  dBbcGeo->set_MaxPmtNo(0,128);

  // Setting dBbcGhitRawPar Parameters
  size_t nrc=1;
  dBbcGhitRawPar->SetRowCount(nrc);
  dBbcGhitRawPar->set_AngleCut(0,45.0);
  dBbcGhitRawPar->set_Nindex(0,1.47);
  dBbcGhitRawPar->set_N0(0,50.0);
  dBbcGhitRawPar->set_MomLowerLim(0,0.00001);
  dBbcGhitRawPar->set_MaxAdc(0,4096.0);
  dBbcGhitRawPar->set_MaxTdc(0,4096.0);
  dBbcGhitRawPar->set_randseed(0,80298);

}

