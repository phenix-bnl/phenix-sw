//************************************************************
// Parameter set-up macro written by the Pyrite GUI for PHOOL 
//************************************************************

{


  // Setting dPad23Par Parameters
  size_t nrc=1;
  dPad23Par->SetRowCount(nrc);
  dPad23Par->set_idatePC23(0,19981225);

  // Setting dPadGeom Parameters
  size_t nrc=1;
  dPadGeom->SetRowCount(nrc);
  dPadGeom->set_pdxoff(0,0,-24.31);
  dPadGeom->set_pdxoff(1,0,-81.2);
  dPadGeom->set_pdxoff(2,0,-95.7);
  dPadGeom->set_pdzoff(0,0,-89.5575);
  dPadGeom->set_pdzoff(1,0,-152.475);
  dPadGeom->set_pdzoff(2,0,-178.69);
  dPadGeom->set_pdgas(0,0,0.60);
  dPadGeom->set_pdgas(1,0,1.00);
  dPadGeom->set_pdgas(2,0,1.20);
  dPadGeom->set_aasep(0,0,0.84);
  dPadGeom->set_aasep(1,0,1.40);
  dPadGeom->set_aasep(2,0,1.65);
  dPadGeom->set_pxlen(0,0,0.82);
  dPadGeom->set_pxlen(1,0,1.375);
  dPadGeom->set_pxlen(2,0,1.622);
  dPadGeom->set_wside(0,0,0.27);
  dPadGeom->set_wside(1,0,0.47);
  dPadGeom->set_wside(2,0,0.55);
  dPadGeom->set_wcent(0,0,0.15);
  dPadGeom->set_wcent(1,0,0.26);
  dPadGeom->set_wcent(2,0,0.31);
  dPadGeom->set_pxsep(0,0,0.025);
  dPadGeom->set_pxsep(1,0,0.025);
  dPadGeom->set_pxsep(2,0,0.025);
  dPadGeom->set_clsep(0,0,0.1);
  dPadGeom->set_clsep(1,0,0.15);
  dPadGeom->set_clsep(2,0,0.2);
  dPadGeom->set_npdsec(0,0,16);
  dPadGeom->set_npdsec(1,0,8);
  dPadGeom->set_npdsec(2,0,8);
  dPadGeom->set_npdwr(0,0,58);
  dPadGeom->set_npdwr(1,0,116);
  dPadGeom->set_npdwr(2,0,116);
  dPadGeom->set_npdx(0,0,20);
  dPadGeom->set_npdx(1,0,40);
  dPadGeom->set_npdx(2,0,40);
  dPadGeom->set_npdz(0,0,216);
  dPadGeom->set_npdz(1,0,216);
  dPadGeom->set_npdz(2,0,216);
  dPadGeom->set_sectperarm(0,0,8);
  dPadGeom->set_sectperarm(1,0,4);
  dPadGeom->set_sectperarm(2,0,4);
  dPadGeom->set_inradius(0,0,248.891);
  dPadGeom->set_inradius(1,0,419.173);
  dPadGeom->set_inradius(2,0,492.012);
  dPadGeom->set_zgap(0,0,0.0);
  dPadGeom->set_zgap(1,0,8.106);
  dPadGeom->set_zgap(2,0,8.106);
  dPadGeom->set_phibote(0,213.75);
  dPadGeom->set_phitope(0,123.75);
  dPadGeom->set_phibotw(0,-33.75);
  dPadGeom->set_phitopw(0,56.25);

  // Setting dPadSlowSimPar Parameters
  size_t nrc=1;
  dPadSlowSimPar->SetRowCount(nrc);
  dPadSlowSimPar->set_verbose(0,0);
  dPadSlowSimPar->set_randseed(0,0,-298574);
  dPadSlowSimPar->set_randseed(1,0,-398574);
  dPadSlowSimPar->set_randseed(2,0,-498574);
  dPadSlowSimPar->set_qnoise(0,0,0.0007);
  dPadSlowSimPar->set_qnoise(1,0,0.002);
  dPadSlowSimPar->set_qnoise(2,0,0.0027);
  dPadSlowSimPar->set_threshold(0,0,0.02);
  dPadSlowSimPar->set_threshold(1,0,0.033);
  dPadSlowSimPar->set_threshold(2,0,0.04);

  // Setting dPadFEMPar Parameters
  size_t nrc=1;
  dPadFEMPar->SetRowCount(nrc);
  dPadFEMPar->set_pcnumber(0,0);
  dPadFEMPar->set_debug(0,0);
  dPadFEMPar->set_fout(0,0);
  dPadFEMPar->set_mode(0,0);
  dPadFEMPar->set_last(0,0);
  dPadFEMPar->set_skipg(0,0);

  // Setting dPadDCMPar Parameters
  size_t nrc=1;
  dPadDCMPar->SetRowCount(nrc);
  dPadDCMPar->set_debug(0,0);
  dPadDCMPar->set_fout(0,0);
  dPadDCMPar->set_scheme(0,0);
  dPadDCMPar->set_idx(0,0);

}

