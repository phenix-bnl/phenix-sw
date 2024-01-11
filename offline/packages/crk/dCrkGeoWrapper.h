#ifndef __DCRKGEOWRAPPER_H__
#define __DCRKGEOWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkGeo.h"
class dCrkGeoWrapper: public PHTable
{
public:
  dCrkGeoWrapper(const char* name = "dCrkGeo", const size_t& max_rows = 1);
//  dCrkGeoWrapper(const dCrkGeoWrapper& source);
//  dCrkGeoWrapper& operator=(const dCrkGeoWrapper& source);

  ~dCrkGeoWrapper();

  void* RawTableData();
  DCRKGEO_ST* TableData();

  DCRKGEO_ST& operator[](const size_t& row);
  const DCRKGEO_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

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
  void set_pmt_phi_min(size_t n, float v) {
    fTableData[n].pmt_phi_min = v;
  }
  float get_pmt_phi_min(size_t n) const {
    return fTableData[n].pmt_phi_min;
  }
  void set_pmt_phi_max(size_t n, float v) {
    fTableData[n].pmt_phi_max = v;
  }
  float get_pmt_phi_max(size_t n) const {
    return fTableData[n].pmt_phi_max;
  }
  void set_pmt_dphi(size_t n, float v) {
    fTableData[n].pmt_dphi = v;
  }
  float get_pmt_dphi(size_t n) const {
    return fTableData[n].pmt_dphi;
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

private:
  DCRKGEO_ST* fTableData;

  ClassDef(dCrkGeoWrapper,1)
};
#endif /*__DCRKGEOWRAPPER_H__*/
