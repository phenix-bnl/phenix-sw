//************************************************************
// Parameter set-up macro for PRDF write (BBC only version) 
//************************************************************

{
  size_t nrc=1;
  dBbcGeo->SetRowCount(nrc);
  dBbcGeo->set_MaxPmtNo(0,128);

  size_t nrc=1;
  dBbcGhitRawPar->SetRowCount(nrc);
  dBbcGhitRawPar->set_AngleCut(0, 45.000000);
  dBbcGhitRawPar->set_Nindex(0, 1.470000);
  dBbcGhitRawPar->set_N0(0, 50.000000);
  dBbcGhitRawPar->set_MomLowerLim(0, 0.000010);
  dBbcGhitRawPar->set_MaxAdc(0, 4096.000000);
  dBbcGhitRawPar->set_MaxTdc(0, 4096.000000);
  dBbcGhitRawPar->set_randseed(0, 80298);

  size_t nrc=1;
  dBbcRawHitPar->SetRowCount(nrc);
  dBbcRawHitPar->set_MinAdc(0, 0);
  dBbcRawHitPar->set_MaxAdc(0, 4096);
  dBbcRawHitPar->set_MinTdc0(0, 10);
  dBbcRawHitPar->set_MaxTdc0(0, 4000);
  dBbcRawHitPar->set_MinTdc1(0, 10);
  dBbcRawHitPar->set_MaxTdc1(0, 4000);

}
