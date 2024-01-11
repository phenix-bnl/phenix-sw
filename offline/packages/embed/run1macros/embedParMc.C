//************************************************************
// Parameter set-up macro written by the Pyrite GUI for PHOOL 
//************************************************************

{
  int nrc=1;
  dDchRecoPar->SetRowCount(nrc);
  dDchRecoPar->set_houghThresholdOnXCell(0,10) ;  // 
  dDchRecoPar->set_houghThresholdOnXMask(0,15) ;  //         
  dDchRecoPar->set_houghThresholdOnUVCell(0,3);  // 
  dDchRecoPar->set_houghThresholdOnUVMask(0,6) ; // 
  dDchRecoPar->set_purgeCandidateThreshold(0,15) ; // 
  dDchRecoPar->set_firstXHoughThreshold(0,10) ;  // mc is 15
  dDchRecoPar->set_secondXHoughThreshold(0,10) ; // mc is 15
  dDchRecoPar->set_minimumNumberOfXHits(0,8);
  dDchRecoPar->set_minimumNumberOfUVHits(0,0);
  dDchRecoPar->set_XHitsThreshold(0,10);
  dDchRecoPar->set_cellDifferenceCut(0,8);
  dDchRecoPar->set_delBetaCut(0,0.2);
  dDchRecoPar->set_deltaBetaCut(0,0.2);
  dDchRecoPar->set_wireResolution(0,0.015); //
  dDchRecoPar->set_initUVChi2(0,10); // 
  dDchRecoPar->set_initXChi2(0,5);  // 
  dDchRecoPar->set_deltaBetaVertexCut(0, 0.5); //  
  dDchRecoPar->set_numberOfAlphaBins(0,300);
  dDchRecoPar->set_numberOfPhiBins(0,6000);
  dDchRecoPar->set_maxAlpha(0,0.8);
  dDchRecoPar->set_minAlpha(0,-0.8);
  dDchRecoPar->set_maxPhi(0,1.);
  dDchRecoPar->set_minPhi(0,-0.65);
  dDchRecoPar->set_numberOfBetaBins(0,60);
  dDchRecoPar->set_numberOfZedBins(0,200);
  dDchRecoPar->set_maxBeta(0,2.5);
  dDchRecoPar->set_minBeta(0,0.6);
  dDchRecoPar->set_maxZed(0,100.);
  dDchRecoPar->set_minZed(0,-100.);
  dDchRecoPar->set_mirrorHitAnalysis(0,0); // 0 for data. 1 for mc
  // Setting dDchGeom Parameters
  int nrc=1;
  dDchGeom->SetRowCount(nrc);
  dDchGeom->set_ncells(0,80);
  dDchGeom->set_ngusset(0,23);
  dDchGeom->set_ti_switch(0,1);
  dDchGeom->set_suppzlength(0,191.0);
  dDchGeom->set_inradius(0,202.0);
  dDchGeom->set_outradius(0,246.0);
  dDchGeom->set_phibotw(0,-33.75);
  dDchGeom->set_phitopw(0,56.25);
  dDchGeom->set_phitope(0,123.75);
  dDchGeom->set_phibote(0,213.75);
  dDchGeom->set_planethick(0,0.3);
  dDchGeom->set_uvangle(0,4.50);
  dDchGeom->set_winthickin(0,0.0025);
  dDchGeom->set_winthickout(0,0.005);
  dDchGeom->set_supptiside(0,1.2);
  dDchGeom->set_suppalside(0,1.5);
  dDchGeom->set_suppzthick(0,27.0);
  dDchGeom->set_supptibase(0,1.5);
  dDchGeom->set_suppalbase(0,2.5);
  dDchGeom->set_x1baserad(0,1.7);
  dDchGeom->set_x2baserad(0,1.7);
  dDchGeom->set_x1basez(0,1.1);
  dDchGeom->set_x2basez(0,1.1);
  dDchGeom->set_x1slotthick(0,0.70);
  dDchGeom->set_x2slotthick(0,0.70);
  dDchGeom->set_x1slotz(0,2.4);
  dDchGeom->set_x2slotz(0,2.4);
  dDchGeom->set_x1suppthick(0,0.4);
  dDchGeom->set_x2suppthick(0,0.4);
  dDchGeom->set_x1suppz(0,4.0);
  dDchGeom->set_x2suppz(0,4.0);
  dDchGeom->set_x1rextent(0,1.5);
  dDchGeom->set_x2rextent(0,1.5);
  dDchGeom->set_u1rextent(0,1.5);
  dDchGeom->set_v1rextent(0,1.5);
  dDchGeom->set_u2rextent(0,1.5);
  dDchGeom->set_v2rextent(0,1.5);
  dDchGeom->set_u1basez(0,0.6);
  dDchGeom->set_v1basez(0,0.6);
  dDchGeom->set_u2basez(0,0.6);
  dDchGeom->set_v2basez(0,0.6);
  dDchGeom->set_u1slotz(0,1.8);
  dDchGeom->set_v1slotz(0,1.8);
  dDchGeom->set_u2slotz(0,1.8);
  dDchGeom->set_v2slotz(0,1.8);
  dDchGeom->set_u1suppz(0,4.0);
  dDchGeom->set_v1suppz(0,4.0);
  dDchGeom->set_u2suppz(0,4.0);
  dDchGeom->set_v2suppz(0,4.0);
  dDchGeom->set_cfibinrad(0,1.27);
  dDchGeom->set_cfiboutrad(0,2.0065);
  dDchGeom->set_propregwidth(0,0.3);
  dDchGeom->set_zvsdfactor(0,0.0975);
  dDchGeom->set_guidewiresep(0,0.2);
  dDchGeom->set_rplane(0,0,204.55);
  dDchGeom->set_rplane(1,0,205.15);
  dDchGeom->set_rplane(2,0,205.75);
  dDchGeom->set_rplane(3,0,206.35);
  dDchGeom->set_rplane(4,0,206.95);
  dDchGeom->set_rplane(5,0,207.55);
  dDchGeom->set_rplane(6,0,208.15);
  dDchGeom->set_rplane(7,0,208.75);
  dDchGeom->set_rplane(8,0,209.35);
  dDchGeom->set_rplane(9,0,209.95);
  dDchGeom->set_rplane(10,0,210.55);
  dDchGeom->set_rplane(11,0,211.15);
  dDchGeom->set_rplane(12,0,214.85);
  dDchGeom->set_rplane(13,0,215.45);
  dDchGeom->set_rplane(14,0,216.05);
  dDchGeom->set_rplane(15,0,216.65);
  dDchGeom->set_rplane(16,0,220.35);
  dDchGeom->set_rplane(17,0,220.95);
  dDchGeom->set_rplane(18,0,221.55);
  dDchGeom->set_rplane(19,0,222.15);
  dDchGeom->set_rplane(20,0,225.85);
  dDchGeom->set_rplane(21,0,226.45);
  dDchGeom->set_rplane(22,0,227.05);
  dDchGeom->set_rplane(23,0,227.65);
  dDchGeom->set_rplane(24,0,228.25);
  dDchGeom->set_rplane(25,0,228.85);
  dDchGeom->set_rplane(26,0,229.45);
  dDchGeom->set_rplane(27,0,230.05);
  dDchGeom->set_rplane(28,0,230.65);
  dDchGeom->set_rplane(29,0,231.25);
  dDchGeom->set_rplane(30,0,231.85);
  dDchGeom->set_rplane(31,0,232.45);
  dDchGeom->set_rplane(32,0,236.15);
  dDchGeom->set_rplane(33,0,236.75);
  dDchGeom->set_rplane(34,0,237.35);
  dDchGeom->set_rplane(35,0,237.95);
  dDchGeom->set_rplane(36,0,241.65);
  dDchGeom->set_rplane(37,0,242.25);
  dDchGeom->set_rplane(38,0,242.85);
  dDchGeom->set_rplane(39,0,243.45);
  dDchGeom->set_rplane(40,0,247.40);
  // Setting dDchFastSimPar Parameters
  int nrc=1;
  dDchFastSimPar->SetRowCount(nrc);
  dDchFastSimPar->set_randseed(0,-376386);
//  dDchFastSimPar->set_wire_eff(0,0.99);
  printf("Setting wire efficiency in new fast sim to 85%\n");
  dDchFastSimPar->set_wire_eff(0,0.85);
  dDchFastSimPar->set_rphires(0,0.015);
  dDchFastSimPar->set_rphiprop(0,0.015);
  dDchFastSimPar->set_twotrksep(0,0.0);
  dDchFastSimPar->set_back_eff(0,0.0);
  dDchFastSimPar->set_verbose(0,0);
  dDchFastSimPar->set_testchain(0,1);

  // Setting padEvtToRaw parameters
  // padEvtToRaw->setDebugLevel(0);

  // Setting padInclBad parameters
  padInclBad->setDebugLevel(0);
  padInclBad->doNotInclBadChs();
  padInclBad->doInclBadROCs();
  padInclBad->RemoveHotROCs();
  padInclBad->RemoveUnSynchROCs();
  padInclBad->doNotAddHotROCs();
  padInclBad->doNotAddInactiveROCs();
  padInclBad->doNotAddUnSynchROCs();

  // Setting dPadRecPar Parameters
  int nrc=1;
  dPadRecPar->SetRowCount(nrc);
  dPadRecPar->set_verbose(0,0);
  dPadRecPar->set_method(0,0,0);
  dPadRecPar->set_method(1,0,0);
  dPadRecPar->set_method(2,0,0);
  // Setting dPad23Par Parameters
  int nrc=1;
  dPad23Par->SetRowCount(nrc);
  dPad23Par->set_idatePC23(0,19981225);

  // Setting dPadGeom Parameters
  int nrc=1;
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
  int nrc=1;
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

  // Setting dPadRecPar Parameters
  int nrc=1;
  dPadRecPar->SetRowCount(nrc);
  dPadRecPar->set_verbose(0,0);
  dPadRecPar->set_method(0,0,0);
  dPadRecPar->set_method(1,0,0);
  dPadRecPar->set_method(2,0,0);

  // Setting mPadDetGeo Parameters
  double pc1Radius = 248.891;
  double pc2Radius = 419.173;
  double pc3Radius = 492.012;
  double ThetaArm[2] = { -0.589049, 2.15984};
  mPadDetGeo->set_pc1Radius(pc1Radius);
  mPadDetGeo->set_pc2Radius(pc2Radius);
  mPadDetGeo->set_pc3Radius(pc3Radius);
  mPadDetGeo->set_Theta0(ThetaArm);

  // Setting PadRecModule parameters
  short level = 0;              // Debug level 0: no debug information
  Pc1Rec->setDebugLevel(level); // Debug level 1: print method calls
  Pc2Rec->setDebugLevel(level); // Debug level 2: show reconstruction statistics
  Pc3Rec->setDebugLevel(level); // Debug level 3: show dPadCluster fillings
                                // Debug level 4: Save hit information in a text file
                                //                -> ./padrec.dat
                                // Debug level 5: Save hit information for Birdseye event display
                                //                -> ./birdseye.dat
  short mode = 0;
  Pc1Rec->setSplitMode(mode); // Split mode 0: Do not split large clusters
  Pc2Rec->setSplitMode(mode); // Split mode 1: Split large clusters
  Pc3Rec->setSplitMode(mode);

  // The maximum allowed cluster size (number of cells)
  short splitMax = 100;
  Pc1Rec->setSplitMax(splitMax);
  Pc2Rec->setSplitMax(splitMax);
  Pc3Rec->setSplitMax(splitMax);

  short limit = 2; // Demand two (default) out of three good pads per cell
  Pc1Rec->setPadTypeLimit(limit);
  Pc2Rec->setPadTypeLimit(limit);
  Pc3Rec->setPadTypeLimit(limit);

  // The idea behind the reconstruction code is to place an imaginative box
  // around each cluster and to measure its size. These values set the maximum
  // size of the box that covers the cluster created by a single particle.
  short oneW = 3; // Box height
  short oneZ = 3; // Box width
  short oneL = 7; // Maximum number of allowed cells within a one particle box
  short twoW = 6;
  short twoZ = 6;
  short twoL = 16;
  short threeW = 8;
  short threeZ = 8;
  short threeL = 24;
  Pc1Rec->setClusterSizes(oneW, oneZ, oneL, twoW, twoZ, twoL, threeW, threeZ, threeL);
  Pc2Rec->setClusterSizes(oneW, oneZ, oneL, twoW, twoZ, twoL, threeW, threeZ, threeL);
  Pc3Rec->setClusterSizes(oneW, oneZ, oneL, twoW, twoZ, twoL, threeW, threeZ, threeL);

  // Setting mPadDetGeo Parameters
  double pc1Radius = 248.891;
  double pc2Radius = 419.173;
  double pc3Radius = 492.012;
  double ThetaArm[2] = { -0.589049, 2.15984};
  mPadDetGeo->set_pc1Radius(pc1Radius);
  mPadDetGeo->set_pc2Radius(pc2Radius);
  mPadDetGeo->set_pc3Radius(pc3Radius);
  mPadDetGeo->set_Theta0(ThetaArm);

  // Setting dTofGeoPar Parameters

  dTofGeoPar->SetRowCount(1);
  dTofGeoPar->set_rslat(0,3.2905);
  dTofGeoPar->set_slat_width(0,1.52654);
  dTofGeoPar->set_scintz(0,0,69.631);
  dTofGeoPar->set_scintz(1,0,11.606);
  dTofGeoPar->set_scintz(2,0,56.609);
  dTofGeoPar->set_rpos(0,0,503.0);
  dTofGeoPar->set_rpos(1,0,503.0);
  dTofGeoPar->set_rpos(2,0,503.0);
  dTofGeoPar->set_rpos(3,0,503.0);
  dTofGeoPar->set_rpos(4,0,503.0);
  dTofGeoPar->set_rpos(5,0,503.0);
  dTofGeoPar->set_rpos(6,0,503.0);
  dTofGeoPar->set_rpos(7,0,503.0);
  dTofGeoPar->set_rpos(8,0,503.0);
  dTofGeoPar->set_rpos(9,0,503.0);
  dTofGeoPar->set_rpos(10,0,0.0);
  dTofGeoPar->set_phi(0,0,180.0);
  dTofGeoPar->set_phi(1,0,180.0);
  dTofGeoPar->set_phi(2,0,180.0);
  dTofGeoPar->set_phi(3,0,180.0);
  dTofGeoPar->set_phi(4,0,180.0);
  dTofGeoPar->set_phi(5,0,180.0);
  dTofGeoPar->set_phi(6,0,180.0);
  dTofGeoPar->set_phi(7,0,180.0);
  dTofGeoPar->set_phi(8,0,202.5);
  dTofGeoPar->set_phi(9,0,202.5);
  dTofGeoPar->set_phi(10,0,0.0);
  dTofGeoPar->set_zpos(0,0,-170.972);
  dTofGeoPar->set_zpos(1,0,-122.123);
  dTofGeoPar->set_zpos(2,0,-73.2739);
  dTofGeoPar->set_zpos(3,0,-24.4246);
  dTofGeoPar->set_zpos(4,0,24.4246);
  dTofGeoPar->set_zpos(5,0,73.2739);
  dTofGeoPar->set_zpos(6,0,122.123);
  dTofGeoPar->set_zpos(7,0,170.972);
  dTofGeoPar->set_zpos(8,0,-24.4246);
  dTofGeoPar->set_zpos(9,0,24.4246);
  dTofGeoPar->set_zpos(10,0,0.0);

  // Setting dTofPerfPar Parameters

  dTofPerfPar->SetRowCount(1);
  dTofPerfPar->set_verbose(0,0);

  // Setting dTofGhitRawPar Parameters
  dTofGhitRawPar->SetRowCount(1);
  dTofGhitRawPar->set_randseed(0,987654);
  dTofGhitRawPar->set_verbose(0,0);
  dTofGhitRawPar->set_min_cell(0,0);
  dTofGhitRawPar->set_max_cell(0,63);
  dTofGhitRawPar->set_min_qvc(0,0);
  dTofGhitRawPar->set_max_qvc(0,4095);
  dTofGhitRawPar->set_min_tvc(0,0);
  dTofGhitRawPar->set_max_tvc(0,4095);

  // Setting dTofRawRecPar Parameters

  dTofRawRecPar->SetRowCount(1);
  dTofRawRecPar->set_verbose(0,0);

  // Setting dTofCalPar Parameters

  dTofCalPar->SetRowCount(1);
  dTofCalPar->set_option(0,0);
  dTofCalPar->set_qvc_corr(0,0,1.0);
  dTofCalPar->set_qvc_corr(1,0,1.0);
  dTofCalPar->set_qvc_corrlsr(0,0,1.0);
  dTofCalPar->set_qvc_corrlsr(1,0,1.0);
  dTofCalPar->set_eloss_conv(0,800.0);
  dTofCalPar->set_eloss_mip(0,0.003);
  dTofCalPar->set_tvc_conv(0,0,27.0);
  dTofCalPar->set_tvc_conv(1,0,27.0);
  dTofCalPar->set_tvc_ped(0,0,4095.0);
  dTofCalPar->set_tvc_ped(1,0,4095.0);
  dTofCalPar->set_t0(0,0,0.0);
  dTofCalPar->set_t0(1,0,0.0);
  dTofCalPar->set_t0_lsr(0,0,0.0);
  dTofCalPar->set_t0_lsr(1,0,0.0);
  dTofCalPar->set_slew_a(0,0,-12.0);
  dTofCalPar->set_slew_a(1,0,-12.0);
  dTofCalPar->set_slew_b(0,0,650.0);
  dTofCalPar->set_slew_b(1,0,650.0);
  dTofCalPar->set_scint_vlight(0,14.0);
  dTofCalPar->set_scint_attenu(0,128);

  // Setting dTofUcalPar Parameters

  dTofUcalPar->SetRowCount(1);
  dTofUcalPar->set_option(0,0);
  dTofUcalPar->set_qvc_chgain(0,0,3750.0);
  dTofUcalPar->set_qvc_chgain(1,0,3750.0);
  dTofUcalPar->set_tvc_conv(0,0,27.0);
  dTofUcalPar->set_tvc_conv(1,0,27.0);
  dTofUcalPar->set_tvc_ped(0,0,4095.0);
  dTofUcalPar->set_tvc_ped(1,0,4095.0);
  dTofUcalPar->set_slew_a(0,0,-12.0);
  dTofUcalPar->set_slew_a(1,0,-12.0);
  dTofUcalPar->set_slew_b(0,0,650.0);
  dTofUcalPar->set_slew_b(1,0,650.0);
  dTofUcalPar->set_scint_vlight(0,14.0);
  dTofUcalPar->set_scint_attenu(0,128.0);
  dTofUcalPar->set_tof_sigma(0,80.0);


  // Setting dCglPidPar Parameters
  int nrc=1;
  dCglPidPar->SetRowCount(nrc);
  dCglPidPar->set_tec(0,1);
  dCglPidPar->set_crk(0,1);
  dCglPidPar->set_emc_t(0,1);
  dCglPidPar->set_emc_e(0,1);
  dCglPidPar->set_tof(0,1);
  dCglPidPar->set_tec_dedx_mip(0,5.7);
  dCglPidPar->set_charge2_cut(0,3.0);
  dCglPidPar->set_n_pid_neg1(0,5);
  dCglPidPar->set_n_pid_pos1(0,7);
  dCglPidPar->set_n_pid_pos2(0,2);
  dCglPidPar->set_pid_neg1(0,0,3);
  dCglPidPar->set_pid_neg1(1,0,6);
  dCglPidPar->set_pid_neg1(2,0,9);
  dCglPidPar->set_pid_neg1(3,0,12);
  dCglPidPar->set_pid_neg1(4,0,15);
  dCglPidPar->set_pid_neg1(5,0,0);
  dCglPidPar->set_pid_neg1(6,0,0);
  dCglPidPar->set_pid_neg1(7,0,0);
  dCglPidPar->set_pid_neg1(8,0,0);
  dCglPidPar->set_pid_neg1(9,0,0);
  dCglPidPar->set_pid_neg1(10,0,0);
  dCglPidPar->set_pid_neg1(11,0,0);
  dCglPidPar->set_pid_neg1(12,0,0);
  dCglPidPar->set_pid_neg1(13,0,0);
  dCglPidPar->set_pid_neg1(14,0,0);
  dCglPidPar->set_pid_neg1(15,0,0);
  dCglPidPar->set_pid_neg1(16,0,0);
  dCglPidPar->set_pid_neg1(17,0,0);
  dCglPidPar->set_pid_neg1(18,0,0);
  dCglPidPar->set_pid_neg1(19,0,0);
  dCglPidPar->set_pid_pos1(0,0,2);
  dCglPidPar->set_pid_pos1(1,0,5);
  dCglPidPar->set_pid_pos1(2,0,8);
  dCglPidPar->set_pid_pos1(3,0,11);
  dCglPidPar->set_pid_pos1(4,0,14);
  dCglPidPar->set_pid_pos1(5,0,45);
  dCglPidPar->set_pid_pos1(6,0,46);
  dCglPidPar->set_pid_pos1(7,0,0);
  dCglPidPar->set_pid_pos1(8,0,0);
  dCglPidPar->set_pid_pos1(9,0,0);
  dCglPidPar->set_pid_pos1(10,0,0);
  dCglPidPar->set_pid_pos1(11,0,0);
  dCglPidPar->set_pid_pos1(12,0,0);
  dCglPidPar->set_pid_pos1(13,0,0);
  dCglPidPar->set_pid_pos1(14,0,0);
  dCglPidPar->set_pid_pos1(15,0,0);
  dCglPidPar->set_pid_pos1(16,0,0);
  dCglPidPar->set_pid_pos1(17,0,0);
  dCglPidPar->set_pid_pos1(18,0,0);
  dCglPidPar->set_pid_pos1(19,0,0);
  dCglPidPar->set_pid_pos2(0,0,49);
  dCglPidPar->set_pid_pos2(1,0,47);
  dCglPidPar->set_pid_pos2(2,0,0);
  dCglPidPar->set_pid_pos2(3,0,0);
  dCglPidPar->set_pid_pos2(4,0,0);
  dCglPidPar->set_pid_pos2(5,0,0);
  dCglPidPar->set_pid_pos2(6,0,0);
  dCglPidPar->set_pid_pos2(7,0,0);
  dCglPidPar->set_pid_pos2(8,0,0);
  dCglPidPar->set_pid_pos2(9,0,0);
  dCglPidPar->set_pid_pos2(10,0,0);
  dCglPidPar->set_pid_pos2(11,0,0);
  dCglPidPar->set_pid_pos2(12,0,0);
  dCglPidPar->set_pid_pos2(13,0,0);
  dCglPidPar->set_pid_pos2(14,0,0);
  dCglPidPar->set_pid_pos2(15,0,0);
  dCglPidPar->set_pid_pos2(16,0,0);
  dCglPidPar->set_pid_pos2(17,0,0);
  dCglPidPar->set_pid_pos2(18,0,0);
  dCglPidPar->set_pid_pos2(19,0,0);
  dCglPidPar->set_mass_neg1(0,0,0.5110e-3);
  dCglPidPar->set_mass_neg1(1,0,0.1057);
  dCglPidPar->set_mass_neg1(2,0,0.1396);
  dCglPidPar->set_mass_neg1(3,0,0.4937);
  dCglPidPar->set_mass_neg1(4,0,0.9383);
  dCglPidPar->set_mass_neg1(5,0,0.0);
  dCglPidPar->set_mass_neg1(6,0,0.0);
  dCglPidPar->set_mass_neg1(7,0,0.0);
  dCglPidPar->set_mass_neg1(8,0,0.0);
  dCglPidPar->set_mass_neg1(9,0,0.0);
  dCglPidPar->set_mass_neg1(10,0,0.0);
  dCglPidPar->set_mass_neg1(11,0,0.0);
  dCglPidPar->set_mass_neg1(12,0,0.0);
  dCglPidPar->set_mass_neg1(13,0,0.0);
  dCglPidPar->set_mass_neg1(14,0,0.0);
  dCglPidPar->set_mass_neg1(15,0,0.0);
  dCglPidPar->set_mass_neg1(16,0,0.0);
  dCglPidPar->set_mass_neg1(17,0,0.0);
  dCglPidPar->set_mass_neg1(18,0,0.0);
  dCglPidPar->set_mass_neg1(19,0,0.0);
  dCglPidPar->set_mass_pos1(0,0,0.511e-3);
  dCglPidPar->set_mass_pos1(1,0,0.1057);
  dCglPidPar->set_mass_pos1(2,0,0.1396);
  dCglPidPar->set_mass_pos1(3,0,0.4937);
  dCglPidPar->set_mass_pos1(4,0,0.9383);
  dCglPidPar->set_mass_pos1(5,0,1.876);
  dCglPidPar->set_mass_pos1(6,0,2.809);
  dCglPidPar->set_mass_pos1(7,0,0.0);
  dCglPidPar->set_mass_pos1(8,0,0.0);
  dCglPidPar->set_mass_pos1(9,0,0.0);
  dCglPidPar->set_mass_pos1(10,0,0.0);
  dCglPidPar->set_mass_pos1(11,0,0.0);
  dCglPidPar->set_mass_pos1(12,0,0.0);
  dCglPidPar->set_mass_pos1(13,0,0.0);
  dCglPidPar->set_mass_pos1(14,0,0.0);
  dCglPidPar->set_mass_pos1(15,0,0.0);
  dCglPidPar->set_mass_pos1(16,0,0.0);
  dCglPidPar->set_mass_pos1(17,0,0.0);
  dCglPidPar->set_mass_pos1(18,0,0.0);
  dCglPidPar->set_mass_pos1(19,0,0.0);
  dCglPidPar->set_mass_pos2(0,0,2.809);
  dCglPidPar->set_mass_pos2(1,0,3.727);
  dCglPidPar->set_mass_pos2(2,0,0.0);
  dCglPidPar->set_mass_pos2(3,0,0.0);
  dCglPidPar->set_mass_pos2(4,0,0.0);
  dCglPidPar->set_mass_pos2(5,0,0.0);
  dCglPidPar->set_mass_pos2(6,0,0.0);
  dCglPidPar->set_mass_pos2(7,0,0.0);
  dCglPidPar->set_mass_pos2(8,0,0.0);
  dCglPidPar->set_mass_pos2(9,0,0.0);
  dCglPidPar->set_mass_pos2(10,0,0.0);
  dCglPidPar->set_mass_pos2(11,0,0.0);
  dCglPidPar->set_mass_pos2(12,0,0.0);
  dCglPidPar->set_mass_pos2(13,0,0.0);
  dCglPidPar->set_mass_pos2(14,0,0.0);
  dCglPidPar->set_mass_pos2(15,0,0.0);
  dCglPidPar->set_mass_pos2(16,0,0.0);
  dCglPidPar->set_mass_pos2(17,0,0.0);
  dCglPidPar->set_mass_pos2(18,0,0.0);
  dCglPidPar->set_mass_pos2(19,0,0.0);
  dCglPidPar->set_tof_res(0,0.085);
  dCglPidPar->set_crk_t_res(0,0.150);
  dCglPidPar->set_tec_bg_res(0,0.2);
  dCglPidPar->set_max_m2_chi2(0,9.0);
  dCglPidPar->set_cm_per_ns_c(0,29.9792458);
  dCglPidPar->set_emc_pcutoff(0,0.35);
  dCglPidPar->set_emc_mcutoff(0,0.08);
  dCglPidPar->set_min_cl_m2(0,0.001);
  dCglPidPar->set_emc_p_e_diff_elec_cut(0,0.2);
  dCglPidPar->set_tec_elec_dedx(0,8.55);
  dCglPidPar->set_tec_dedx_elec_cut(0,2.0);
  dCglPidPar->set_crk_npmt_elec_cut(0,3.0);
  dCglPidPar->set_crk_chi2_elec_cut(0,4.0);
  dCglPidPar->set_crk_rdisp_elec_cut(0,4.0);
  dCglPidPar->set_emc_antib_cut(0,0.8);
  dCglPidPar->set_bbc_nom_res(0,0.04);
  dCglPidPar->set_emc_nom_tres(0,0.3);
  dCglPidPar->set_emc_nom_eres(0,0,0.078);
  dCglPidPar->set_emc_nom_eres(1,0,0.015);
  dCglPidPar->set_cgl_nom_pres(0,0.015);
  dCglPidPar->set_nom_fpath_tof(0,490.0);
  dCglPidPar->set_nom_fpath_emc(0,500.0);
  dCglPidPar->set_nom_fpath_crk(0,300.0);
  dCglPidPar->set_nom_dfpath(0,0.02);
  dCglPidPar->set_verbose(0,0);

  // Setting cglDetectorGeo Parameters
  short cglDetectorGeo_Verbose = 0;
  //cglDetectorGeo->set_Verbose(cglDetectorGeo_Verbose);
  //double cglDetectorGeo_xyz0[3] = {0.0,0.0,0.0};
  //cglDetectorGeo->set_xyz0(cglDetectorGeo_xyz0);
  //double cglDetectorGeo_xyz0East[3] = {0.0,0.0,0.0};
  //if (geomFlag == 0) cglDetectorGeo_xyz0East[0] = -44.0;
  //cglDetectorGeo->set_xyz0East(cglDetectorGeo_xyz0East);
  //double cglDetectorGeo_xyz0West[3] = {0.0,0.0,0.0};
  //if (geomFlag == 0) cglDetectorGeo_xyz0West[0] = 41.0;
  //cglDetectorGeo->set_xyz0West(cglDetectorGeo_xyz0West);
  // Julia's geometry set-up (alignment)
  PHPoint wo(-41,0,0);
  PHPoint eo(44,0,0);
  PHVector Xaxis(1,0,0);
  PHVector Yaxis(0,1,0);
  PHFrame eastF(eo,Xaxis,Yaxis);
  PHFrame westF(wo,Xaxis,Yaxis);
  if (geomFlag == 0) cglDetectorGeo->set_eastFrame(eastF);
  if (geomFlag == 0) cglDetectorGeo->set_westFrame(westF);
  // end Julia's geometry set-up
  double cglDetectorGeo_pc1Radius = 248.891;
  cglDetectorGeo->set_pc1Radius(cglDetectorGeo_pc1Radius);
  double cglDetectorGeo_pc2Radius = 419.173;
  cglDetectorGeo->set_pc2Radius(cglDetectorGeo_pc2Radius);
  double cglDetectorGeo_pc3Radius = 492.012;
  cglDetectorGeo->set_pc3Radius(cglDetectorGeo_pc3Radius);
  double cglDetectorGeo_dchRadius = 220.0;
  cglDetectorGeo->set_dchRadius(cglDetectorGeo_dchRadius);
  double cglDetectorGeo_crkRadius = 260.0;
  cglDetectorGeo->set_crkRadius(cglDetectorGeo_crkRadius);
  double cglDetectorGeo_tecRadius = 430.0;
  cglDetectorGeo->set_tecRadius(cglDetectorGeo_tecRadius);
  double cglDetectorGeo_tofRadius = 503.0;
  cglDetectorGeo->set_tofRadius(cglDetectorGeo_tofRadius);
  //SCJ double cglDetectorGeo_pbscRadius = 510.0;
  double cglDetectorGeo_pbscRadius = 507.2;
  cglDetectorGeo->set_pbscRadius(cglDetectorGeo_pbscRadius);
  double cglDetectorGeo_pbglRadius = 540.0;
  cglDetectorGeo->set_pbglRadius(cglDetectorGeo_pbglRadius);
  double cglDetectorGeo_pc1ZWidth = 180.0;
  cglDetectorGeo->set_pc1ZWidth(cglDetectorGeo_pc1ZWidth);
  double cglDetectorGeo_pc2ZWidth = 310.0;
  cglDetectorGeo->set_pc2ZWidth(cglDetectorGeo_pc2ZWidth);
  double cglDetectorGeo_pc3ZWidth = 360.0;
  cglDetectorGeo->set_pc3ZWidth(cglDetectorGeo_pc3ZWidth);
  double cglDetectorGeo_dchZWidth = 90.0;
  cglDetectorGeo->set_dchZWidth(cglDetectorGeo_dchZWidth);
  double cglDetectorGeo_crkZWidth = 200.0;
  cglDetectorGeo->set_crkZWidth(cglDetectorGeo_crkZWidth);
  double cglDetectorGeo_tecZWidth = 320.0;
  cglDetectorGeo->set_tecZWidth(cglDetectorGeo_tecZWidth);
  double cglDetectorGeo_tofZWidth = 380.0;
  cglDetectorGeo->set_tofZWidth(cglDetectorGeo_tofZWidth);
  double cglDetectorGeo_pbscZWidth = 390.0;
  cglDetectorGeo->set_pbscZWidth(cglDetectorGeo_pbscZWidth);
  double cglDetectorGeo_pbglZWidth = 390.0;
  cglDetectorGeo->set_pbglZWidth(cglDetectorGeo_pbglZWidth);
  double cglDetectorGeo_Theta0[2] = {2.15984,-0.589049};
  cglDetectorGeo->set_Theta0(cglDetectorGeo_Theta0);
  double cglDetectorGeo_dThetaArm[2] = {90.0,90.0};
  cglDetectorGeo->set_dThetaArm(cglDetectorGeo_dThetaArm);
  short cglDetectorGeo_dchActive0 = 1;
  cglDetectorGeo->set_dchActive(0,cglDetectorGeo_dchActive0);
  short cglDetectorGeo_dchActive1 = 1;
  cglDetectorGeo->set_dchActive(1,cglDetectorGeo_dchActive1);
  short cglDetectorGeo_crkActive0 = 1;
  cglDetectorGeo->set_crkActive(0,cglDetectorGeo_crkActive0);
  short cglDetectorGeo_crkActive1 = 1;
  cglDetectorGeo->set_crkActive(1,cglDetectorGeo_crkActive1);
  short cglDetectorGeo_pc1Active0[8] = {1,1,1,1,1,1,1,1};
  cglDetectorGeo->set_pc1Active(0,cglDetectorGeo_pc1Active0);
  short cglDetectorGeo_pc1Active1[8] = {1,1,1,1,1,1,1,1};
  cglDetectorGeo->set_pc1Active(1,cglDetectorGeo_pc1Active1);
  short cglDetectorGeo_pc2Active0[4] = {1,1,1,1};
  cglDetectorGeo->set_pc2Active(0,cglDetectorGeo_pc2Active0);
  short cglDetectorGeo_pc2Active1[4] = {1,1,1,1};
  cglDetectorGeo->set_pc2Active(1,cglDetectorGeo_pc2Active1);
  short cglDetectorGeo_pc3Active0[4] = {1,1,1,1};
  cglDetectorGeo->set_pc3Active(0,cglDetectorGeo_pc3Active0);
  short cglDetectorGeo_pc3Active1[4] = {1,1,1,1};
  cglDetectorGeo->set_pc3Active(1,cglDetectorGeo_pc3Active1);
  short cglDetectorGeo_tecActive0[4] = {1,1,1,1};
  cglDetectorGeo->set_tecActive(0,cglDetectorGeo_tecActive0);
  short cglDetectorGeo_tecActive1[4] = {0,0,0,0};
  cglDetectorGeo->set_tecActive(1,cglDetectorGeo_tecActive1);
  short cglDetectorGeo_tofActive0[4] = {1,1,1,1};
  cglDetectorGeo->set_tofActive(0,cglDetectorGeo_tofActive0);
  short cglDetectorGeo_tofActive1[4] = {0,0,0,0};
  cglDetectorGeo->set_tofActive(1,cglDetectorGeo_tofActive1);
  short cglDetectorGeo_pbscActive0[4] = {1,1,1,1};
  cglDetectorGeo->set_pbscActive(0,cglDetectorGeo_pbscActive0);
  short cglDetectorGeo_pbscActive1[4] = {1,1,1,1};
  cglDetectorGeo->set_pbscActive(1,cglDetectorGeo_pbscActive1);
  short cglDetectorGeo_pbglActive0[4] = {1,1,1,1};
  cglDetectorGeo->set_pbglActive(0,cglDetectorGeo_pbglActive0);
  short cglDetectorGeo_pbglActive1[4] = {1,1,1,1};
  cglDetectorGeo->set_pbglActive(1,cglDetectorGeo_pbglActive1);

  // Setting dCglProjPar Parameters
  int     nrc=1;
  dCglProjPar->SetRowCount(nrc);
  dCglProjPar->set_mode(0,1);
  dCglProjPar->set_verbose(0,0);
  dCglProjPar->set_rad_tof(0,500.0);
  dCglProjPar->set_rad_emc(0,510.0);
  dCglProjPar->set_rad_crk(0,259.0);
  dCglProjPar->set_rad_pc2(0,420.0);
  dCglProjPar->set_rad_pc3(0,490.0);
  dCglProjPar->set_rad_mom(0,280.0);

  // Setting up mPHDchTrackModel parameters
  //dchTrackModel->setTrackQualityThresh(0);

  // Setting cglHitAssociate Parameters
  short cglHitAssociate_Verbose = 0;
  cglHitAssociate->set_Verbose(cglHitAssociate_Verbose);
  short cglHitAssociate_TrackModelFlag = 0;
  if (bFieldFlag == 0) cglHitAssociate_TrackModelFlag = 0;
  if (bFieldFlag == 1) cglHitAssociate_TrackModelFlag = 2;
  cglHitAssociate->set_TrackModelFlag(cglHitAssociate_TrackModelFlag);
  double cglHitAssociate_TECZ0Buffer = 10.0;
  cglHitAssociate->set_TECZ0Buffer(cglHitAssociate_TECZ0Buffer);
  double cglHitAssociate_TECSlopeCut = 1.0;
  cglHitAssociate->set_TECSlopeCut(cglHitAssociate_TECSlopeCut);
  short cglHitAssociate_RemoveHits = 0;
  cglHitAssociate->set_RemoveHits(cglHitAssociate_RemoveHits);
  //SCJ  short cglHitAssociate_UseFlag[10] = {0,0,1,1,1,0,1,1,1,1};
  short cglHitAssociate_UseFlag[10] = {1,1,1,1,1,1,1,1,1,1};
  cglHitAssociate->set_UseFlag(cglHitAssociate_UseFlag);
 double cglHitAssociate_PhiRoad[10] = {0.0,0.0,30.0,15.0,15.0,0.0,15.0,15.0,15.0,15.0};
  cglHitAssociate->set_PhiRoad(cglHitAssociate_PhiRoad);
  double cglHitAssociate_ZRoad[10] = {0.0,0.0,40.0,25.0,25.0,25.0,25.0,25.0,25.0,25.0};
  cglHitAssociate->set_ZRoad(cglHitAssociate_ZRoad);
  double cglHitAssociate_MinPhiWidth = 0.12;
  cglHitAssociate->set_MinPhiWidth(cglHitAssociate_MinPhiWidth);
  double cglHitAssociate_MinZWidth = 10000.0;
  cglHitAssociate->set_MinZWidth(cglHitAssociate_MinZWidth);
  short cglHitAssociate_PredictMomentum = 1;
  if (bFieldFlag == 0) cglHitAssociate_PredictMomentum = 0;
  if (bFieldFlag == 1) cglHitAssociate_PredictMomentum = 1;
  cglHitAssociate->set_PredictMomentum(cglHitAssociate_PredictMomentum);
  short cglHitAssociate_MinDchQuality = 0;
  cglHitAssociate->set_MinDchQuality(cglHitAssociate_MinDchQuality);
  short cglHitAssociate_PhiRoadOnly = 1;
  cglHitAssociate->set_PhiRoadOnly(cglHitAssociate_PhiRoadOnly);
  short cglHitAssociate_MaxDchQuality = 4;
  cglHitAssociate->set_MaxDchQuality(cglHitAssociate_MaxDchQuality);
  cglHitAssociate->PrintParameters();
}

