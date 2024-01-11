//************************************************************
// Parameter set-up macro written by the Pyrite GUI for PHOOL 
//************************************************************

{

  // Setting dPadGeom Parameters
  size_t nrc=1;
  //dPadGeom->SetRowCount(nrc);
  //dPadGeom->set_pdxoff(0,0,-24.31);
  //dPadGeom->set_pdxoff(1,0,-81.2);
  //dPadGeom->set_pdxoff(2,0,-95.7);
  //dPadGeom->set_pdzoff(0,0,-89.5575);
  //dPadGeom->set_pdzoff(1,0,-152.475);
  //dPadGeom->set_pdzoff(2,0,-178.69);
  //dPadGeom->set_pdgas(0,0,0.60);
  //dPadGeom->set_pdgas(1,0,1.00);
  //dPadGeom->set_pdgas(2,0,1.20);
  //dPadGeom->set_aasep(0,0,0.84);
  //dPadGeom->set_aasep(1,0,1.40);
  //dPadGeom->set_aasep(2,0,1.65);
  //dPadGeom->set_pxlen(0,0,0.82);
  //dPadGeom->set_pxlen(1,0,1.375);
  //dPadGeom->set_pxlen(2,0,1.622);
  //dPadGeom->set_wside(0,0,0.27);
  //dPadGeom->set_wside(1,0,0.47);
  //dPadGeom->set_wside(2,0,0.55);
  //dPadGeom->set_wcent(0,0,0.15);
  //dPadGeom->set_wcent(1,0,0.26);
  //dPadGeom->set_wcent(2,0,0.31);
  //dPadGeom->set_pxsep(0,0,0.025);
  //dPadGeom->set_pxsep(1,0,0.025);
  //dPadGeom->set_pxsep(2,0,0.025);
  //dPadGeom->set_clsep(0,0,0.1);
  //dPadGeom->set_clsep(1,0,0.15);
  //dPadGeom->set_clsep(2,0,0.2);
  //dPadGeom->set_npdsec(0,0,16);
  //dPadGeom->set_npdsec(1,0,8);
  //dPadGeom->set_npdsec(2,0,8);
  //dPadGeom->set_npdwr(0,0,58);
  //dPadGeom->set_npdwr(1,0,116);
  //dPadGeom->set_npdwr(2,0,116);
  //dPadGeom->set_npdx(0,0,20);
  //dPadGeom->set_npdx(1,0,40);
  //dPadGeom->set_npdx(2,0,40);
  //dPadGeom->set_npdz(0,0,216);
  //dPadGeom->set_npdz(1,0,216);
  //dPadGeom->set_npdz(2,0,216);
  //dPadGeom->set_sectperarm(0,0,8);
  //dPadGeom->set_sectperarm(1,0,4);
  //dPadGeom->set_sectperarm(2,0,4);
  //dPadGeom->set_inradius(0,0,248.891);
  //dPadGeom->set_inradius(1,0,419.173);
  //dPadGeom->set_inradius(2,0,492.012);
  //dPadGeom->set_zgap(0,0,0.0);
  //dPadGeom->set_zgap(1,0,8.106);
  //dPadGeom->set_zgap(2,0,8.106);
  //dPadGeom->set_phibote(0,213.75);
  //dPadGeom->set_phitope(0,123.75);
  //dPadGeom->set_phibotw(0,-33.75);
  //dPadGeom->set_phitopw(0,56.25);

  // Setting up parameters for padDetectorGeo
  double xyzOrigin[3] = {0.0, 0.0, 0.0};
  double ThetaArm[2] = {-0.589049, 2.15984};
  mPadDetGeo->set_Verbose(1);
  mPadDetGeo->set_xyz0(xyzOrigin);
  mPadDetGeo->set_Theta0(ThetaArm);

  // Setting up PC1 parameters for padDetectorGeo
  double pc1Radius = 248.256;
  double pc1ZWidth = 90.0;
  short pc1Active[8] = {1,1,1,1,1,1,1,1};
  //mPadDetGeo->set_pc1Radius(pc1Radius);
  //mPadDetGeo->set_pc1ZWidth(pc1ZWidth);
  //mPadDetGeo->set_pc1Active(0,pc1Active);
  //mPadDetGeo->set_pc1Active(1,pc1Active);

  // Setting up PC2 parameters for padDetectorGeo
  double pc2Radius = 419.18;
  double pc2ZWidth = 155.0;
  short pc2Active[4] = {1,1,1,1};
  //mPadDetGeo->set_pc2Radius(pc2Radius);
  //mPadDetGeo->set_pc2ZWidth(pc2ZWidth);
  //mPadDetGeo->set_pc2Active(0,pc2Active);
  //mPadDetGeo->set_pc2Active(1,pc2Active);

  // Setting up PC3 parameters for padDetectorGeo
  double pc3Radius = 490.0;
  double pc3ZWidth = 180.0;
  short pc3Active[4] = {1,1,1,1};
  //mPadDetGeo->set_pc3Radius(pc3Radius);
  //mPadDetGeo->set_pc3ZWidth(pc3ZWidth);
  //mPadDetGeo->set_pc3Active(0,pc3Active);
  //mPadDetGeo->set_pc3Active(1,pc3Active);

}
