#ifndef __MUNGEOWRAPPER_H__
#define __MUNGEOWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "mungeo.h"
class mungeoWrapper: public PHTable
{
public:
  mungeoWrapper(const char* name = "mungeo", const size_t& max_rows = 1);
//  mungeoWrapper(const mungeoWrapper& source);
//  mungeoWrapper& operator=(const mungeoWrapper& source);

  ~mungeoWrapper();

  void* RawTableData();
  MUNGEO_ST* TableData();

  MUNGEO_ST& operator[](const size_t& row);
  const MUNGEO_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_nr_muon_arms(size_t n, short v) {
    fTableData[n].nr_muon_arms = v;
  }
  short get_nr_muon_arms(size_t n) const {
    return fTableData[n].nr_muon_arms;
  }
  void set_nr_muid_planes(size_t n, short v) {
    fTableData[n].nr_muid_planes = v;
  }
  short get_nr_muid_planes(size_t n) const {
    return fTableData[n].nr_muid_planes;
  }
  void set_ndet_per_pl(size_t n, short v) {
    fTableData[n].ndet_per_pl = v;
  }
  short get_ndet_per_pl(size_t n) const {
    return fTableData[n].ndet_per_pl;
  }
  void set_mu_floor_flg(size_t n, short v) {
    fTableData[n].mu_floor_flg = v;
  }
  short get_mu_floor_flg(size_t n) const {
    return fTableData[n].mu_floor_flg;
  }
  void set_muid_zmin(size_t n, float v) {
    fTableData[n].muid_zmin = v;
  }
  float get_muid_zmin(size_t n) const {
    return fTableData[n].muid_zmin;
  }
  void set_muid_zmax(size_t n, float v) {
    fTableData[n].muid_zmax = v;
  }
  float get_muid_zmax(size_t n) const {
    return fTableData[n].muid_zmax;
  }
  void set_muid_ang(size_t n, float v) {
    fTableData[n].muid_ang = v;
  }
  float get_muid_ang(size_t n) const {
    return fTableData[n].muid_ang;
  }
  void set_beam_height(size_t n, float v) {
    fTableData[n].beam_height = v;
  }
  float get_beam_height(size_t n) const {
    return fTableData[n].beam_height;
  }
  void set_mu_top_height(size_t n, float v) {
    fTableData[n].mu_top_height = v;
  }
  float get_mu_top_height(size_t n) const {
    return fTableData[n].mu_top_height;
  }
  void set_mu_floor_height(size_t n, float v) {
    fTableData[n].mu_floor_height = v;
  }
  float get_mu_floor_height(size_t n) const {
    return fTableData[n].mu_floor_height;
  }
  void set_mu_strp_thick(size_t n, float v) {
    fTableData[n].mu_strp_thick = v;
  }
  float get_mu_strp_thick(size_t n) const {
    return fTableData[n].mu_strp_thick;
  }
  void set_mu_gas_thick(size_t n, float v) {
    fTableData[n].mu_gas_thick = v;
  }
  float get_mu_gas_thick(size_t n) const {
    return fTableData[n].mu_gas_thick;
  }
  void set_mu_plas_thick(size_t n, float v) {
    fTableData[n].mu_plas_thick = v;
  }
  float get_mu_plas_thick(size_t n) const {
    return fTableData[n].mu_plas_thick;
  }
  void set_mu_yoke_thick(size_t d0, size_t n, float v) {
    fTableData[n].mu_yoke_thick[d0] = v;
  }
  float get_mu_yoke_thick(size_t d0, size_t n) const {
    return fTableData[n].mu_yoke_thick[d0];
  }
  void set_mu_donut_thick1(size_t d0, size_t n, float v) {
    fTableData[n].mu_donut_thick1[d0] = v;
  }
  float get_mu_donut_thick1(size_t d0, size_t n) const {
    return fTableData[n].mu_donut_thick1[d0];
  }
  void set_mu_donut_thick2(size_t d0, size_t n, float v) {
    fTableData[n].mu_donut_thick2[d0] = v;
  }
  float get_mu_donut_thick2(size_t d0, size_t n) const {
    return fTableData[n].mu_donut_thick2[d0];
  }
  void set_rmax1_donut1(size_t d0, size_t n, float v) {
    fTableData[n].rmax1_donut1[d0] = v;
  }
  float get_rmax1_donut1(size_t d0, size_t n) const {
    return fTableData[n].rmax1_donut1[d0];
  }
  void set_rmax1_donut2(size_t d0, size_t n, float v) {
    fTableData[n].rmax1_donut2[d0] = v;
  }
  float get_rmax1_donut2(size_t d0, size_t n) const {
    return fTableData[n].rmax1_donut2[d0];
  }
  void set_rmax2_donut1(size_t d0, size_t n, float v) {
    fTableData[n].rmax2_donut1[d0] = v;
  }
  float get_rmax2_donut1(size_t d0, size_t n) const {
    return fTableData[n].rmax2_donut1[d0];
  }
  void set_rmax2_donut2(size_t d0, size_t n, float v) {
    fTableData[n].rmax2_donut2[d0] = v;
  }
  float get_rmax2_donut2(size_t d0, size_t n) const {
    return fTableData[n].rmax2_donut2[d0];
  }
  void set_mu_floor_thick(size_t n, float v) {
    fTableData[n].mu_floor_thick = v;
  }
  float get_mu_floor_thick(size_t n) const {
    return fTableData[n].mu_floor_thick;
  }
  void set_mu_abs_thick(size_t d0, size_t n, float v) {
    fTableData[n].mu_abs_thick[d0] = v;
  }
  float get_mu_abs_thick(size_t d0, size_t n) const {
    return fTableData[n].mu_abs_thick[d0];
  }
  void set_mu_hi_abs(size_t d0, size_t n, float v) {
    fTableData[n].mu_hi_abs[d0] = v;
  }
  float get_mu_hi_abs(size_t d0, size_t n) const {
    return fTableData[n].mu_hi_abs[d0];
  }
  void set_mu_hi_med(size_t d0, size_t n, short v) {
    fTableData[n].mu_hi_med[d0] = v;
  }
  short get_mu_hi_med(size_t d0, size_t n) const {
    return fTableData[n].mu_hi_med[d0];
  }
  void set_mu_lo_med(size_t d0, size_t n, short v) {
    fTableData[n].mu_lo_med[d0] = v;
  }
  short get_mu_lo_med(size_t d0, size_t n) const {
    return fTableData[n].mu_lo_med[d0];
  }
  void set_nmed_mu_ps(size_t n, short v) {
    fTableData[n].nmed_mu_ps = v;
  }
  short get_nmed_mu_ps(size_t n) const {
    return fTableData[n].nmed_mu_ps;
  }
  void set_nmed_mu_gas(size_t n, short v) {
    fTableData[n].nmed_mu_gas = v;
  }
  short get_nmed_mu_gas(size_t n) const {
    return fTableData[n].nmed_mu_gas;
  }
  void set_nmed_mu_cs(size_t n, short v) {
    fTableData[n].nmed_mu_cs = v;
  }
  short get_nmed_mu_cs(size_t n) const {
    return fTableData[n].nmed_mu_cs;
  }
  void set_nmed_mu_sh(size_t n, short v) {
    fTableData[n].nmed_mu_sh = v;
  }
  short get_nmed_mu_sh(size_t n) const {
    return fTableData[n].nmed_mu_sh;
  }
  void set_nmed_mu_sd(size_t n, short v) {
    fTableData[n].nmed_mu_sd = v;
  }
  short get_nmed_mu_sd(size_t n) const {
    return fTableData[n].nmed_mu_sd;
  }
  void set_nmed_mudn(size_t n, short v) {
    fTableData[n].nmed_mudn = v;
  }
  short get_nmed_mudn(size_t n) const {
    return fTableData[n].nmed_mudn;
  }
  void set_nmed_muhl(size_t n, short v) {
    fTableData[n].nmed_muhl = v;
  }
  short get_nmed_muhl(size_t n) const {
    return fTableData[n].nmed_muhl;
  }
  void set_nmed_mufl(size_t n, short v) {
    fTableData[n].nmed_mufl = v;
  }
  short get_nmed_mufl(size_t n) const {
    return fTableData[n].nmed_mufl;
  }
  void set_nmed_yoke(size_t n, short v) {
    fTableData[n].nmed_yoke = v;
  }
  short get_nmed_yoke(size_t n) const {
    return fTableData[n].nmed_yoke;
  }
  void set_mu_hi_color(size_t n, short v) {
    fTableData[n].mu_hi_color = v;
  }
  short get_mu_hi_color(size_t n) const {
    return fTableData[n].mu_hi_color;
  }
  void set_mu_lo_color(size_t n, short v) {
    fTableData[n].mu_lo_color = v;
  }
  short get_mu_lo_color(size_t n) const {
    return fTableData[n].mu_lo_color;
  }
  void set_color_muid(size_t n, short v) {
    fTableData[n].color_muid = v;
  }
  short get_color_muid(size_t n) const {
    return fTableData[n].color_muid;
  }
  void set_color_hole(size_t n, short v) {
    fTableData[n].color_hole = v;
  }
  short get_color_hole(size_t n) const {
    return fTableData[n].color_hole;
  }
  void set_color_dont(size_t n, short v) {
    fTableData[n].color_dont = v;
  }
  short get_color_dont(size_t n) const {
    return fTableData[n].color_dont;
  }
  void set_color_floor(size_t n, short v) {
    fTableData[n].color_floor = v;
  }
  short get_color_floor(size_t n) const {
    return fTableData[n].color_floor;
  }
  void set_color_strd(size_t n, short v) {
    fTableData[n].color_strd = v;
  }
  short get_color_strd(size_t n) const {
    return fTableData[n].color_strd;
  }
  void set_color_yoke(size_t n, short v) {
    fTableData[n].color_yoke = v;
  }
  short get_color_yoke(size_t n) const {
    return fTableData[n].color_yoke;
  }
  void set_rykmin1(size_t d0, size_t n, float v) {
    fTableData[n].rykmin1[d0] = v;
  }
  float get_rykmin1(size_t d0, size_t n) const {
    return fTableData[n].rykmin1[d0];
  }
  void set_rykmin2(size_t d0, size_t n, float v) {
    fTableData[n].rykmin2[d0] = v;
  }
  float get_rykmin2(size_t d0, size_t n) const {
    return fTableData[n].rykmin2[d0];
  }
  void set_rmin_donut(size_t d0, size_t n, float v) {
    fTableData[n].rmin_donut[d0] = v;
  }
  float get_rmin_donut(size_t d0, size_t n) const {
    return fTableData[n].rmin_donut[d0];
  }
  void set_zyoke(size_t d0, size_t n, float v) {
    fTableData[n].zyoke[d0] = v;
  }
  float get_zyoke(size_t d0, size_t n) const {
    return fTableData[n].zyoke[d0];
  }
  void set_zgap_yoke_abs(size_t n, float v) {
    fTableData[n].zgap_yoke_abs = v;
  }
  float get_zgap_yoke_abs(size_t n) const {
    return fTableData[n].zgap_yoke_abs;
  }
  void set_zgap_labs_ldet(size_t n, float v) {
    fTableData[n].zgap_labs_ldet = v;
  }
  float get_zgap_labs_ldet(size_t n) const {
    return fTableData[n].zgap_labs_ldet;
  }
  void set_muid_delx(size_t n, float v) {
    fTableData[n].muid_delx = v;
  }
  float get_muid_delx(size_t n) const {
    return fTableData[n].muid_delx;
  }
  void set_str_xstag(size_t n, float v) {
    fTableData[n].str_xstag = v;
  }
  float get_str_xstag(size_t n) const {
    return fTableData[n].str_xstag;
  }
  void set_muhl_shld_flag(size_t n, short v) {
    fTableData[n].muhl_shld_flag = v;
  }
  short get_muhl_shld_flag(size_t n) const {
    return fTableData[n].muhl_shld_flag;
  }
  void set_color_muhl_shld(size_t n, short v) {
    fTableData[n].color_muhl_shld = v;
  }
  short get_color_muhl_shld(size_t n) const {
    return fTableData[n].color_muhl_shld;
  }
  void set_nmed_muhl_shld(size_t n, short v) {
    fTableData[n].nmed_muhl_shld = v;
  }
  short get_nmed_muhl_shld(size_t n) const {
    return fTableData[n].nmed_muhl_shld;
  }
  void set_z_muhl_shld(size_t d0, size_t n, float v) {
    fTableData[n].z_muhl_shld[d0] = v;
  }
  float get_z_muhl_shld(size_t d0, size_t n) const {
    return fTableData[n].z_muhl_shld[d0];
  }
  void set_thick_muhl_shld(size_t n, float v) {
    fTableData[n].thick_muhl_shld = v;
  }
  float get_thick_muhl_shld(size_t n) const {
    return fTableData[n].thick_muhl_shld;
  }
  void set_muhl_config_flag(size_t n, short v) {
    fTableData[n].muhl_config_flag = v;
  }
  short get_muhl_config_flag(size_t n) const {
    return fTableData[n].muhl_config_flag;
  }
  void set_muabs_config_flag(size_t n, short v) {
    fTableData[n].muabs_config_flag = v;
  }
  short get_muabs_config_flag(size_t n) const {
    return fTableData[n].muabs_config_flag;
  }
  void set_npl_muhl(size_t n, short v) {
    fTableData[n].npl_muhl = v;
  }
  short get_npl_muhl(size_t n) const {
    return fTableData[n].npl_muhl;
  }
  void set_z_muhl(size_t d0, size_t n, float v) {
    fTableData[n].z_muhl[d0] = v;
  }
  float get_z_muhl(size_t d0, size_t n) const {
    return fTableData[n].z_muhl[d0];
  }
  void set_rmin_muhl(size_t d0, size_t n, float v) {
    fTableData[n].rmin_muhl[d0] = v;
  }
  float get_rmin_muhl(size_t d0, size_t n) const {
    return fTableData[n].rmin_muhl[d0];
  }
  void set_rmax_muhl(size_t d0, size_t n, float v) {
    fTableData[n].rmax_muhl[d0] = v;
  }
  float get_rmax_muhl(size_t d0, size_t n) const {
    return fTableData[n].rmax_muhl[d0];
  }

private:
  MUNGEO_ST* fTableData;

  ClassDef(mungeoWrapper,1)
};
#endif /*__MUNGEOWRAPPER_H__*/
