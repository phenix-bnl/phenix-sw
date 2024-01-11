#ifndef __CRKPARWRAPPER_H__
#define __CRKPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "crkpar.h"
class crkparWrapper: public PHTable
{
public:
  crkparWrapper(const char* name = "crkpar", const size_t& max_rows = 1);
//  crkparWrapper(const crkparWrapper& source);
//  crkparWrapper& operator=(const crkparWrapper& source);

  ~crkparWrapper();

  void* RawTableData();
  CRKPAR_ST* TableData();

  CRKPAR_ST& operator[](const size_t& row);
  const CRKPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_pntr(size_t n, short v) {
    fTableData[n].pntr = v;
  }
  short get_pntr(size_t n) const {
    return fTableData[n].pntr;
  }
  void set_phi_cntr(size_t n, float v) {
    fTableData[n].phi_cntr = v;
  }
  float get_phi_cntr(size_t n) const {
    return fTableData[n].phi_cntr;
  }
  void set_phi_open(size_t n, float v) {
    fTableData[n].phi_open = v;
  }
  float get_phi_open(size_t n) const {
    return fTableData[n].phi_open;
  }
  void set_dphi_carm(size_t n, float v) {
    fTableData[n].dphi_carm = v;
  }
  float get_dphi_carm(size_t n) const {
    return fTableData[n].dphi_carm;
  }
  void set_dphi_cshe(size_t n, float v) {
    fTableData[n].dphi_cshe = v;
  }
  float get_dphi_cshe(size_t n) const {
    return fTableData[n].dphi_cshe;
  }
  void set_dphi_cpho(size_t n, float v) {
    fTableData[n].dphi_cpho = v;
  }
  float get_dphi_cpho(size_t n) const {
    return fTableData[n].dphi_cpho;
  }
  void set_phi_cpho_off(size_t n, float v) {
    fTableData[n].phi_cpho_off = v;
  }
  float get_phi_cpho_off(size_t n) const {
    return fTableData[n].phi_cpho_off;
  }
  void set_n_sect(size_t n, short v) {
    fTableData[n].n_sect = v;
  }
  short get_n_sect(size_t n) const {
    return fTableData[n].n_sect;
  }
  void set_n_spm(size_t n, short v) {
    fTableData[n].n_spm = v;
  }
  short get_n_spm(size_t n) const {
    return fTableData[n].n_spm;
  }
  void set_n_pmt(size_t n, short v) {
    fTableData[n].n_pmt = v;
  }
  short get_n_pmt(size_t n) const {
    return fTableData[n].n_pmt;
  }
  void set_r_pmt_ent(size_t n, float v) {
    fTableData[n].r_pmt_ent = v;
  }
  float get_r_pmt_ent(size_t n) const {
    return fTableData[n].r_pmt_ent;
  }
  void set_dx_pmt(size_t d0, size_t n, float v) {
    fTableData[n].dx_pmt[d0] = v;
  }
  float get_dx_pmt(size_t d0, size_t n) const {
    return fTableData[n].dx_pmt[d0];
  }
  void set_r_pmt(size_t d0, size_t n, float v) {
    fTableData[n].r_pmt[d0] = v;
  }
  float get_r_pmt(size_t d0, size_t n) const {
    return fTableData[n].r_pmt[d0];
  }
  void set_z_pmt(size_t d0, size_t n, float v) {
    fTableData[n].z_pmt[d0] = v;
  }
  float get_z_pmt(size_t d0, size_t n) const {
    return fTableData[n].z_pmt[d0];
  }
  void set_theta_pmt(size_t d0, size_t n, float v) {
    fTableData[n].theta_pmt[d0] = v;
  }
  float get_theta_pmt(size_t d0, size_t n) const {
    return fTableData[n].theta_pmt[d0];
  }
  void set_mir_rin(size_t n, float v) {
    fTableData[n].mir_rin = v;
  }
  float get_mir_rin(size_t n) const {
    return fTableData[n].mir_rin;
  }
  void set_mir_thck(size_t n, float v) {
    fTableData[n].mir_thck = v;
  }
  float get_mir_thck(size_t n) const {
    return fTableData[n].mir_thck;
  }
  void set_mir_theta1(size_t n, float v) {
    fTableData[n].mir_theta1 = v;
  }
  float get_mir_theta1(size_t n) const {
    return fTableData[n].mir_theta1;
  }
  void set_mir_theta2(size_t n, float v) {
    fTableData[n].mir_theta2 = v;
  }
  float get_mir_theta2(size_t n) const {
    return fTableData[n].mir_theta2;
  }
  void set_mir_thetacut(size_t n, float v) {
    fTableData[n].mir_thetacut = v;
  }
  float get_mir_thetacut(size_t n) const {
    return fTableData[n].mir_thetacut;
  }
  void set_mir_phi1(size_t n, float v) {
    fTableData[n].mir_phi1 = v;
  }
  float get_mir_phi1(size_t n) const {
    return fTableData[n].mir_phi1;
  }
  void set_mir_phi2(size_t n, float v) {
    fTableData[n].mir_phi2 = v;
  }
  float get_mir_phi2(size_t n) const {
    return fTableData[n].mir_phi2;
  }
  void set_mir_dz(size_t n, float v) {
    fTableData[n].mir_dz = v;
  }
  float get_mir_dz(size_t n) const {
    return fTableData[n].mir_dz;
  }
  void set_wi1_rin(size_t n, float v) {
    fTableData[n].wi1_rin = v;
  }
  float get_wi1_rin(size_t n) const {
    return fTableData[n].wi1_rin;
  }
  void set_wi1_thck(size_t n, float v) {
    fTableData[n].wi1_thck = v;
  }
  float get_wi1_thck(size_t n) const {
    return fTableData[n].wi1_thck;
  }
  void set_wi1_zend(size_t n, float v) {
    fTableData[n].wi1_zend = v;
  }
  float get_wi1_zend(size_t n) const {
    return fTableData[n].wi1_zend;
  }
  void set_wi2_rin(size_t n, float v) {
    fTableData[n].wi2_rin = v;
  }
  float get_wi2_rin(size_t n) const {
    return fTableData[n].wi2_rin;
  }
  void set_wi2_thck(size_t n, float v) {
    fTableData[n].wi2_thck = v;
  }
  float get_wi2_thck(size_t n) const {
    return fTableData[n].wi2_thck;
  }
  void set_wi2_zend(size_t n, float v) {
    fTableData[n].wi2_zend = v;
  }
  float get_wi2_zend(size_t n) const {
    return fTableData[n].wi2_zend;
  }
  void set_tr1_rin(size_t n, float v) {
    fTableData[n].tr1_rin = v;
  }
  float get_tr1_rin(size_t n) const {
    return fTableData[n].tr1_rin;
  }
  void set_tr1_thck(size_t n, float v) {
    fTableData[n].tr1_thck = v;
  }
  float get_tr1_thck(size_t n) const {
    return fTableData[n].tr1_thck;
  }
  void set_tr1_zend(size_t n, float v) {
    fTableData[n].tr1_zend = v;
  }
  float get_tr1_zend(size_t n) const {
    return fTableData[n].tr1_zend;
  }
  void set_tr2_rin(size_t n, float v) {
    fTableData[n].tr2_rin = v;
  }
  float get_tr2_rin(size_t n) const {
    return fTableData[n].tr2_rin;
  }
  void set_tr2_thck(size_t n, float v) {
    fTableData[n].tr2_thck = v;
  }
  float get_tr2_thck(size_t n) const {
    return fTableData[n].tr2_thck;
  }
  void set_tr2_zend(size_t n, float v) {
    fTableData[n].tr2_zend = v;
  }
  float get_tr2_zend(size_t n) const {
    return fTableData[n].tr2_zend;
  }

private:
  CRKPAR_ST* fTableData;

  ClassDef(crkparWrapper,1)
};
#endif /*__CRKPARWRAPPER_H__*/
