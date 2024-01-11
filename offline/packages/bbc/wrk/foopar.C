//************************************************************
// Parameter set-up macro written by the Pyrite GUI for PHOOL 
//************************************************************

{


  // Setting dBbcGeo Parameters
  size_t nrc=1;
  dBbcGeo->SetRowCount(nrc);
  dBbcGeo->set_MaxPmtNo(0,128);

  // Setting dBbcRawHitPar Parameters
  size_t nrc=1;
  dBbcRawHitPar->SetRowCount(nrc);
  dBbcRawHitPar->set_MinAdc(0,0);
  dBbcRawHitPar->set_MaxAdc(0,4096);
  dBbcRawHitPar->set_MinTdc0(0,10);
  dBbcRawHitPar->set_MaxTdc0(0,4000);
  dBbcRawHitPar->set_MinTdc1(0,10);
  dBbcRawHitPar->set_MaxTdc1(0,4000);

}

