//******************************************************************
// Parameter set-up macro for PRDF test write ( CRK version only )
//******************************************************************

{
  // Setting dCrkGhitRawPar Parameters
  size_t nrc=1;
  dCrkGhitRawPar->SetRowCount(nrc);
  dCrkGhitRawPar->set_ghitraw(0,0);
  dCrkGhitRawPar->set_max_adc(0,1024);
  dCrkGhitRawPar->set_max_tdc(0,1024);
  dCrkGhitRawPar->set_min_tdc(0,0);
  dCrkGhitRawPar->set_N0_pisa(0,200.000000);
  dCrkGhitRawPar->set_sinTmax(0,0.500000);

  // Setting dCrkRawFEMpar Parameters
  size_t nrc=1;
  dCrkRawFEMpar->SetRowCount(nrc);
  dCrkRawFEMpar->set_mplex(0,1);
  dCrkRawFEMpar->set_fem_mask(0,65535);

  // Setting dCrkDCMpar Parameters
  size_t nrc=1;
  dCrkDCMpar->SetRowCount(nrc);
  dCrkDCMpar->set_mode(0,1);
  dCrkDCMpar->set_threshold(0,0);

  // Setting dCrkRawHitPar Parameters
  size_t nrc=1;
  dCrkRawHitPar->SetRowCount(nrc);
  dCrkRawHitPar->set_min_pe(0,0.1);

  // Setting dCrkProjPidPar Parameters
  size_t nrc=1;
  dCrkProjPidPar->SetRowCount(nrc);
  //  dCrkProjPidPar->set_in_accept(0,0);
  //  dCrkProjPidPar->set_min_npmt(0,0);
  dCrkProjPidPar->set_in_accept(0,1);
  dCrkProjPidPar->set_min_npmt(0,1);
  dCrkProjPidPar->set_gas(0,3);
  //  dCrkProjPidPar->set_Rmax(0,0.0);
  //  dCrkProjPidPar->set_Rmin(0,0.0);
  //  dCrkProjPidPar->set_Rmax2(0,0.0);
  //  dCrkProjPidPar->set_R0(0,0.0);

  // Setting dCrkDCMRawPar Parameters
  //size_t nrc=1;
  //dCrkDCMRawPar->SetRowCount(nrc);
  //dCrkDCMRawPar->set_threshold(0,0);

}
