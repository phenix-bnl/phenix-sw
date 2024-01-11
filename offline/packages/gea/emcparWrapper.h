#ifndef __EMCPARWRAPPER_H__
#define __EMCPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "emcpar.h"
class emcparWrapper: public PHTable
{
public:
  emcparWrapper(const char* name = "emcpar", const size_t& max_rows = 1);
//  emcparWrapper(const emcparWrapper& source);
//  emcparWrapper& operator=(const emcparWrapper& source);

  ~emcparWrapper();

  void* RawTableData();
  EMCPAR_ST* TableData();

  EMCPAR_ST& operator[](const size_t& row);
  const EMCPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_emc_walls(size_t n, float v) {
    fTableData[n].emc_walls = v;
  }
  float get_emc_walls(size_t n) const {
    return fTableData[n].emc_walls;
  }
  void set_emc_opt(size_t n, float v) {
    fTableData[n].emc_opt = v;
  }
  float get_emc_opt(size_t n) const {
    return fTableData[n].emc_opt;
  }
  void set_iwall(size_t n, float v) {
    fTableData[n].iwall = v;
  }
  float get_iwall(size_t n) const {
    return fTableData[n].iwall;
  }
  void set_itype(size_t n, float v) {
    fTableData[n].itype = v;
  }
  float get_itype(size_t n) const {
    return fTableData[n].itype;
  }
  void set_angle(size_t n, float v) {
    fTableData[n].angle = v;
  }
  float get_angle(size_t n) const {
    return fTableData[n].angle;
  }
  void set_rpos(size_t n, float v) {
    fTableData[n].rpos = v;
  }
  float get_rpos(size_t n) const {
    return fTableData[n].rpos;
  }
  void set_zc_start(size_t n, float v) {
    fTableData[n].zc_start = v;
  }
  float get_zc_start(size_t n) const {
    return fTableData[n].zc_start;
  }
  void set_yc_start(size_t n, float v) {
    fTableData[n].yc_start = v;
  }
  float get_yc_start(size_t n) const {
    return fTableData[n].yc_start;
  }
  void set_lsiz(size_t n, float v) {
    fTableData[n].lsiz = v;
  }
  float get_lsiz(size_t n) const {
    return fTableData[n].lsiz;
  }
  void set_tsiz(size_t n, float v) {
    fTableData[n].tsiz = v;
  }
  float get_tsiz(size_t n) const {
    return fTableData[n].tsiz;
  }
  void set_no_modz(size_t n, float v) {
    fTableData[n].no_modz = v;
  }
  float get_no_modz(size_t n) const {
    return fTableData[n].no_modz;
  }
  void set_no_mody(size_t n, float v) {
    fTableData[n].no_mody = v;
  }
  float get_no_mody(size_t n) const {
    return fTableData[n].no_mody;
  }
  void set_no_smodz(size_t n, float v) {
    fTableData[n].no_smodz = v;
  }
  float get_no_smodz(size_t n) const {
    return fTableData[n].no_smodz;
  }
  void set_no_smody(size_t n, float v) {
    fTableData[n].no_smody = v;
  }
  float get_no_smody(size_t n) const {
    return fTableData[n].no_smody;
  }
  void set_scint_emc_med(size_t n, float v) {
    fTableData[n].scint_emc_med = v;
  }
  float get_scint_emc_med(size_t n) const {
    return fTableData[n].scint_emc_med;
  }
  void set_emc_debug(size_t n, float v) {
    fTableData[n].emc_debug = v;
  }
  float get_emc_debug(size_t n) const {
    return fTableData[n].emc_debug;
  }
  void set_gcuts(size_t d0, size_t n, float v) {
    fTableData[n].gcuts[d0] = v;
  }
  float get_gcuts(size_t d0, size_t n) const {
    return fTableData[n].gcuts[d0];
  }
  void set_emc_r_min_sc(size_t n, float v) {
    fTableData[n].emc_r_min_sc = v;
  }
  float get_emc_r_min_sc(size_t n) const {
    return fTableData[n].emc_r_min_sc;
  }
  void set_emc_r_max_sc(size_t n, float v) {
    fTableData[n].emc_r_max_sc = v;
  }
  float get_emc_r_max_sc(size_t n) const {
    return fTableData[n].emc_r_max_sc;
  }
  void set_emc_r_step(size_t n, float v) {
    fTableData[n].emc_r_step = v;
  }
  float get_emc_r_step(size_t n) const {
    return fTableData[n].emc_r_step;
  }
  void set_emc_z_min(size_t n, float v) {
    fTableData[n].emc_z_min = v;
  }
  float get_emc_z_min(size_t n) const {
    return fTableData[n].emc_z_min;
  }
  void set_emc_z_max(size_t n, float v) {
    fTableData[n].emc_z_max = v;
  }
  float get_emc_z_max(size_t n) const {
    return fTableData[n].emc_z_max;
  }
  void set_emc_z_step(size_t n, float v) {
    fTableData[n].emc_z_step = v;
  }
  float get_emc_z_step(size_t n) const {
    return fTableData[n].emc_z_step;
  }
  void set_emc_x_min_sc(size_t n, float v) {
    fTableData[n].emc_x_min_sc = v;
  }
  float get_emc_x_min_sc(size_t n) const {
    return fTableData[n].emc_x_min_sc;
  }
  void set_emc_x_max_sc(size_t n, float v) {
    fTableData[n].emc_x_max_sc = v;
  }
  float get_emc_x_max_sc(size_t n) const {
    return fTableData[n].emc_x_max_sc;
  }
  void set_emc_x_step(size_t n, float v) {
    fTableData[n].emc_x_step = v;
  }
  float get_emc_x_step(size_t n) const {
    return fTableData[n].emc_x_step;
  }
  void set_emc_dele_max_sc(size_t n, float v) {
    fTableData[n].emc_dele_max_sc = v;
  }
  float get_emc_dele_max_sc(size_t n) const {
    return fTableData[n].emc_dele_max_sc;
  }
  void set_emc_dele_step_sc(size_t n, float v) {
    fTableData[n].emc_dele_step_sc = v;
  }
  float get_emc_dele_step_sc(size_t n) const {
    return fTableData[n].emc_dele_step_sc;
  }
  void set_emc_tof_min(size_t n, float v) {
    fTableData[n].emc_tof_min = v;
  }
  float get_emc_tof_min(size_t n) const {
    return fTableData[n].emc_tof_min;
  }
  void set_emc_tof_max(size_t n, float v) {
    fTableData[n].emc_tof_max = v;
  }
  float get_emc_tof_max(size_t n) const {
    return fTableData[n].emc_tof_max;
  }
  void set_emc_tof_step(size_t n, float v) {
    fTableData[n].emc_tof_step = v;
  }
  float get_emc_tof_step(size_t n) const {
    return fTableData[n].emc_tof_step;
  }
  void set_emc_ind1_max_sc(size_t n, float v) {
    fTableData[n].emc_ind1_max_sc = v;
  }
  float get_emc_ind1_max_sc(size_t n) const {
    return fTableData[n].emc_ind1_max_sc;
  }
  void set_emc_ind2_max_sc(size_t n, float v) {
    fTableData[n].emc_ind2_max_sc = v;
  }
  float get_emc_ind2_max_sc(size_t n) const {
    return fTableData[n].emc_ind2_max_sc;
  }
  void set_emc_iwall_max(size_t n, float v) {
    fTableData[n].emc_iwall_max = v;
  }
  float get_emc_iwall_max(size_t n) const {
    return fTableData[n].emc_iwall_max;
  }
  void set_emc_itype_max(size_t n, float v) {
    fTableData[n].emc_itype_max = v;
  }
  float get_emc_itype_max(size_t n) const {
    return fTableData[n].emc_itype_max;
  }
  void set_emc_i1_max(size_t n, float v) {
    fTableData[n].emc_i1_max = v;
  }
  float get_emc_i1_max(size_t n) const {
    return fTableData[n].emc_i1_max;
  }
  void set_emc_itrack_max(size_t n, float v) {
    fTableData[n].emc_itrack_max = v;
  }
  float get_emc_itrack_max(size_t n) const {
    return fTableData[n].emc_itrack_max;
  }
  void set_emc_spart_max(size_t n, float v) {
    fTableData[n].emc_spart_max = v;
  }
  float get_emc_spart_max(size_t n) const {
    return fTableData[n].emc_spart_max;
  }
  void set_emc_ncycle_max(size_t n, float v) {
    fTableData[n].emc_ncycle_max = v;
  }
  float get_emc_ncycle_max(size_t n) const {
    return fTableData[n].emc_ncycle_max;
  }
  void set_emc_cutgam(size_t n, float v) {
    fTableData[n].emc_cutgam = v;
  }
  float get_emc_cutgam(size_t n) const {
    return fTableData[n].emc_cutgam;
  }
  void set_emc_cutele(size_t n, float v) {
    fTableData[n].emc_cutele = v;
  }
  float get_emc_cutele(size_t n) const {
    return fTableData[n].emc_cutele;
  }
  void set_emc_cutneu(size_t n, float v) {
    fTableData[n].emc_cutneu = v;
  }
  float get_emc_cutneu(size_t n) const {
    return fTableData[n].emc_cutneu;
  }
  void set_emc_cuthad(size_t n, float v) {
    fTableData[n].emc_cuthad = v;
  }
  float get_emc_cuthad(size_t n) const {
    return fTableData[n].emc_cuthad;
  }
  void set_emc_cutmuo(size_t n, float v) {
    fTableData[n].emc_cutmuo = v;
  }
  float get_emc_cutmuo(size_t n) const {
    return fTableData[n].emc_cutmuo;
  }
  void set_array(size_t d0, size_t n, float v) {
    fTableData[n].array[d0] = v;
  }
  float get_array(size_t d0, size_t n) const {
    return fTableData[n].array[d0];
  }

private:
  EMCPAR_ST* fTableData;

  ClassDef(emcparWrapper,1)
};
#endif /*__EMCPARWRAPPER_H__*/
