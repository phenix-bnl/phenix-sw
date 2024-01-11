//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void padgeorun2() {

  // Executing initialization and parameter macros
  gROOT->Macro("padgeoini2.C");
  gROOT->Macro("padgeopar2.C");

  mainIter.cd();
  PHNodeReset reset;
  gROOT->cd();

  // Fetch the geometry from the ASCII file
  mPadDetGeo->FetchFromFile();

  // Reset dPadGeom parameters
  mPadDetGeo->set_pc1Radius(pc1Radius);
  mPadDetGeo->set_pc2Radius(pc2Radius);
  mPadDetGeo->set_pc3Radius(pc3Radius);
  mPadDetGeo->set_xyz0(xyzOrigin);
  mPadDetGeo->set_Theta0(ThetaArm);

  // Print the results
  mPadDetGeo->PrintParams();
  mPadDetGeo->PrintGeo(0,0);
  mPadDetGeo->PrintGeo(0,1);
  mPadDetGeo->PrintGeo(1,0);
  mPadDetGeo->PrintGeo(1,1);
  mPadDetGeo->PrintGeo(2,0);
  mPadDetGeo->PrintGeo(2,1);

  mPadDetGeo->Fetch_dPadGeom(dPadGeom);

  printf("dPadGeom contents\n");
  printf("  PC1 pdxoff = %f\n", dPadGeom->get_pdxoff(0,0));
  printf("  PC2 pdxoff = %f\n", dPadGeom->get_pdxoff(1,0));
  printf("  PC3 pdxoff = %f\n", dPadGeom->get_pdxoff(2,0));
  printf("  PC1 pdzoff = %f\n", dPadGeom->get_pdzoff(0,0));
  printf("  PC2 pdzoff = %f\n", dPadGeom->get_pdzoff(1,0));
  printf("  PC3 pdzoff = %f\n", dPadGeom->get_pdzoff(2,0));
  printf("  PC1 pdgas = %f\n", dPadGeom->get_pdgas(0,0));
  printf("  PC2 pdgas = %f\n", dPadGeom->get_pdgas(1,0));
  printf("  PC3 pdgas = %f\n", dPadGeom->get_pdgas(2,0));
  printf("  PC1 aasep = %f\n", dPadGeom->get_aasep(0,0));
  printf("  PC2 aasep = %f\n", dPadGeom->get_aasep(1,0));
  printf("  PC3 aasep = %f\n", dPadGeom->get_aasep(2,0));
  printf("  PC1 pxlen = %f\n", dPadGeom->get_pxlen(0,0));
  printf("  PC2 pxlen = %f\n", dPadGeom->get_pxlen(1,0));
  printf("  PC3 pxlen = %f\n", dPadGeom->get_pxlen(2,0));
  printf("  PC1 wside = %f\n", dPadGeom->get_wside(0,0));
  printf("  PC2 wside = %f\n", dPadGeom->get_wside(1,0));
  printf("  PC3 wside = %f\n", dPadGeom->get_wside(2,0));
  printf("  PC1 wcent = %f\n", dPadGeom->get_wcent(0,0));
  printf("  PC2 wcent = %f\n", dPadGeom->get_wcent(1,0));
  printf("  PC3 wcent = %f\n", dPadGeom->get_wcent(2,0));
  printf("  PC1 pxsep = %f\n", dPadGeom->get_pxsep(0,0));
  printf("  PC2 pxsep = %f\n", dPadGeom->get_pxsep(1,0));
  printf("  PC3 pxsep = %f\n", dPadGeom->get_pxsep(2,0));
  printf("  PC1 clsep = %f\n", dPadGeom->get_clsep(0,0));
  printf("  PC2 clsep = %f\n", dPadGeom->get_clsep(1,0));
  printf("  PC3 clsep = %f\n", dPadGeom->get_clsep(2,0));
  printf("  PC1 npdsec = %d\n", dPadGeom->get_npdsec(0,0));
  printf("  PC2 npdsec = %d\n", dPadGeom->get_npdsec(1,0));
  printf("  PC3 npdsec = %d\n", dPadGeom->get_npdsec(2,0));
  printf("  PC1 npdwr = %d\n", dPadGeom->get_npdwr(0,0));
  printf("  PC2 npdwr = %d\n", dPadGeom->get_npdwr(1,0));
  printf("  PC3 npdwr = %d\n", dPadGeom->get_npdwr(2,0));
  printf("  PC1 npdx = %d\n", dPadGeom->get_npdx(0,0));
  printf("  PC2 npdx = %d\n", dPadGeom->get_npdx(1,0));
  printf("  PC3 npdx = %d\n", dPadGeom->get_npdx(2,0));
  printf("  PC1 npdz = %d\n", dPadGeom->get_npdz(0,0));
  printf("  PC2 npdz = %d\n", dPadGeom->get_npdz(1,0));
  printf("  PC3 npdz = %d\n", dPadGeom->get_npdz(2,0));
  printf("  PC1 sectperarm = %d\n", dPadGeom->get_sectperarm(0,0));
  printf("  PC2 sectperarm = %d\n", dPadGeom->get_sectperarm(1,0));
  printf("  PC3 sectperarm = %d\n", dPadGeom->get_sectperarm(2,0));
  printf("  PC1 inradius = %f\n", dPadGeom->get_inradius(0,0));
  printf("  PC2 inradius = %f\n", dPadGeom->get_inradius(1,0));
  printf("  PC3 inradius = %f\n", dPadGeom->get_inradius(2,0));
  printf("  PC1 zgap = %f\n", dPadGeom->get_zgap(0,0));
  printf("  PC2 zgap = %f\n", dPadGeom->get_zgap(1,0));
  printf("  PC3 zgap = %f\n", dPadGeom->get_zgap(2,0));
  printf("  phibote = %f\n", dPadGeom->get_phibote(0));
  printf("  phitope = %f\n", dPadGeom->get_phitope(0));
  printf("  phibotw = %f\n", dPadGeom->get_phibotw(0));
  printf("  phitopw = %f\n", dPadGeom->get_phitopw(0));

}

