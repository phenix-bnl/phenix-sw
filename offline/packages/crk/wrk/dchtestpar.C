//************************************************************
// Parameter set-up macro for PRDF write (Dch only version) 
//************************************************************

{
  // Provisional version based on DST creation macro dchpar.C version
  // Add necessary modules for detector response
  // Still contains event reconstruction modules which will not normally be part of PRDF macro
  // These event reconstruction modules are for Quality Assurance checking
  //
  // Should not be necessary to keep repeating the specification size_t nrc each time?
  //

  // Setting dDchEvalPar Parameters
  size_t nrc=1;
  dDchEvalPar->SetRowCount(nrc);
  dDchEvalPar->set_verbose(0,1);

  // Setting dDchDCMPar Parameters
  size_t nrc=1;
  dDchDCMPar->SetRowCount(nrc);
  dDchDCMPar->set_scheme(0,0);

  // Setting dDchFastSimPar Parameters
  size_t nrc=1;
  dDchFastSimPar->SetRowCount(nrc);
  dDchFastSimPar->set_randseed(0,-376386);
  dDchFastSimPar->set_wire_eff(0,0.99);
  dDchFastSimPar->set_rphires(0,0.015);
  dDchFastSimPar->set_rphiprop(0,0.015);
  dDchFastSimPar->set_twotrksep(0,0.0);
  dDchFastSimPar->set_back_eff(0,0.0);
  dDchFastSimPar->set_verbose(0,0);

  // Setting dDchGeom Parameters
  size_t nrc=1;
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

}

