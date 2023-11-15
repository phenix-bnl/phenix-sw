#include "MuiPISAPara.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// MuiPISAPara.cc                                                       //
//                                                                      //
// Implementation of Mui Parameters in PISA , T. K. Ghosh, Vanderbilt   //
//        07. 13.99                                                     //
//////////////////////////////////////////////////////////////////////////

ClassImp(MuiPISAPara)

MuiPISAPara::MuiPISAPara(const Int_t iData [], const Float_t fData [])

{
  int i, p = 0;

  //nr_muon_arms = iData[p++];
  nr_muon_arms = 2;
  nr_muid_planes = iData[p++];
  ndet_per_pl = iData[p++];
  mu_floor_flg = iData[p++];
  muid_zmin = fData[p++];
  muid_zmax = fData[p++];
  muid_ang = fData[p++];
  beam_height = fData[p++];
  mu_top_height = fData[p++];
  mu_floor_height = fData[p++];
  mu_strp_thick = fData[p++];
  mu_gas_thick = fData[p++];
  mu_plas_thick = fData[p++];
  for (i = 0; i < nr_muon_arms; i++) {
    mu_yoke_thick[i] = fData[p++];
  }
  for (i = 0; i < nr_muon_arms; i++) {
    mu_donut_thick1[i] = fData[p++];
    mu_donut_thick2[i] = fData[p++];
  }
  for (i = 0; i < nr_muon_arms; i++) {
    rmax1_donut1[i] = fData[p++];
    rmax1_donut2[i] = fData[p++];
    rmax2_donut1[i] = fData[p++];
    rmax2_donut2[i] = fData[p++];
  }
  mu_floor_thick = fData[p++];
  for (i = 0; i < 11; i++) {
    mu_abs_thick[i] = fData[p++];
  }
  for (i = 0; i < 11; i++) {
    mu_hi_abs[i] = fData[p++];
  }
  for (i = 0; i < 11; i++) {
    mu_hi_med[i] = iData[p++];
  }
  for (i = 0; i < 11; i++) {
    mu_lo_med[i] = iData[p++];
  }
  nmed_mu_ps = iData[p++];
  nmed_mu_gas = iData[p++];
  nmed_mu_cs = iData[p++];
  nmed_mu_sh = iData[p++];
  nmed_mu_sd = iData[p++];
  nmed_mudn = iData[p++];
  nmed_muhl = iData[p++];
  nmed_mufl = iData[p++];
  nmed_yoke = iData[p++];
  mu_hi_color = iData[p++];
  mu_lo_color = iData[p++];
  color_muid = iData[p++];
  color_hole = iData[p++];
  color_dont = iData[p++];
  color_floor = iData[p++];
  color_strd = iData[p++];
  color_yoke = iData[p++];
  for (i = 0; i < nr_muon_arms; i++) {
    rykmin1[i] = fData[p++];
  }
  for (i = 0; i < nr_muon_arms; i++) {
    rykmin2[i] = fData[p++];
  }
  for (i = 0; i < nr_muon_arms; i++) {
    rmin_donut[i] = fData[p++];
  }
  for (i = 0; i < nr_muon_arms; i++) {
    zyoke[i] = fData[p++];
  }
  zgap_yoke_abs = fData[p++];
  zgap_labs_ldet = fData[p++];
  muid_delx = fData[p++];
  str_xstag = fData[p++];
  muhl_shld_flag = iData[p++];
  color_muhl_shld = iData[p++];
  nmed_muhl_shld = iData[p++];
  z_muhl_shld[0] = fData[p++];
  z_muhl_shld[1] = fData[p++];
  thick_muhl_shld = fData[p++];
  muhl_config_flag = iData[p++];
  muabs_config_flag = iData[p++];
  npl_muhl = iData[p++];
  for (i = 0; i < 6; i++) {
    z_muhl[i] = fData[p++];
  }
  for (i = 0; i < 6; i++) {
    rmin_muhl[i] = fData[p++];
  }
 for (i = 0; i < 6; i++) {
    rmin_muhl[i] = fData[p++];
  }
  for (i = 0; i < 6; i++) {
    rmax_muhl[i] = fData[p++];
  }


}









