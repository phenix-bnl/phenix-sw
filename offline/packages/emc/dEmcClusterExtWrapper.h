#ifndef __DEMCCLUSTEREXTWRAPPER_H__
#define __DEMCCLUSTEREXTWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcClusterExt.h"
class dEmcClusterExtWrapper: public PHTable
{
public:
  dEmcClusterExtWrapper(const char* name = "dEmcClusterExt", const size_t& max_rows = 1);
//  dEmcClusterExtWrapper(const dEmcClusterExtWrapper& source);
//  dEmcClusterExtWrapper& operator=(const dEmcClusterExtWrapper& source);

  ~dEmcClusterExtWrapper();

  void* RawTableData();
  DEMCCLUSTEREXT_ST* TableData();

  DEMCCLUSTEREXT_ST& operator[](const size_t& row);
  const DEMCCLUSTEREXT_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_runno(size_t n, int v) {
    fTableData[n].runno = v;
  }
  int get_runno(size_t n) const {
    return fTableData[n].runno;
  }
  void set_evno(size_t n, int v) {
    fTableData[n].evno = v;
  }
  int get_evno(size_t n) const {
    return fTableData[n].evno;
  }
  void set_clusno(size_t n, short v) {
    fTableData[n].clusno = v;
  }
  short get_clusno(size_t n) const {
    return fTableData[n].clusno;
  }
  void set_method(size_t n, short v) {
    fTableData[n].method = v;
  }
  short get_method(size_t n) const {
    return fTableData[n].method;
  }
  void set_type(size_t n, short v) {
    fTableData[n].type = v;
  }
  short get_type(size_t n) const {
    return fTableData[n].type;
  }
  void set_arm(size_t n, short v) {
    fTableData[n].arm = v;
  }
  short get_arm(size_t n) const {
    return fTableData[n].arm;
  }
  void set_sector(size_t n, short v) {
    fTableData[n].sector = v;
  }
  short get_sector(size_t n) const {
    return fTableData[n].sector;
  }
  void set_xyz(size_t d0, size_t n, float v) {
    fTableData[n].xyz[d0] = v;
  }
  float get_xyz(size_t d0, size_t n) const {
    return fTableData[n].xyz[d0];
  }
  void set_dxyz(size_t d0, size_t n, float v) {
    fTableData[n].dxyz[d0] = v;
  }
  float get_dxyz(size_t d0, size_t n) const {
    return fTableData[n].dxyz[d0];
  }
  void set_e(size_t n, float v) {
    fTableData[n].e = v;
  }
  float get_e(size_t n) const {
    return fTableData[n].e;
  }
  void set_ecorr(size_t n, float v) {
    fTableData[n].ecorr = v;
  }
  float get_ecorr(size_t n) const {
    return fTableData[n].ecorr;
  }
  void set_de(size_t n, float v) {
    fTableData[n].de = v;
  }
  float get_de(size_t n) const {
    return fTableData[n].de;
  }
  void set_tof(size_t n, float v) {
    fTableData[n].tof = v;
  }
  float get_tof(size_t n) const {
    return fTableData[n].tof;
  }
  void set_ecent(size_t n, float v) {
    fTableData[n].ecent = v;
  }
  float get_ecent(size_t n) const {
    return fTableData[n].ecent;
  }
  void set_tofcorr(size_t n, float v) {
    fTableData[n].tofcorr = v;
  }
  float get_tofcorr(size_t n) const {
    return fTableData[n].tofcorr;
  }
  void set_dtof(size_t n, float v) {
    fTableData[n].dtof = v;
  }
  float get_dtof(size_t n) const {
    return fTableData[n].dtof;
  }
  void set_qual(size_t n, float v) {
    fTableData[n].qual = v;
  }
  float get_qual(size_t n) const {
    return fTableData[n].qual;
  }
  void set_pid(size_t n, float v) {
    fTableData[n].pid = v;
  }
  float get_pid(size_t n) const {
    return fTableData[n].pid;
  }
  void set_prob_photon(size_t n, float v) {
    fTableData[n].prob_photon = v;
  }
  float get_prob_photon(size_t n) const {
    return fTableData[n].prob_photon;
  }
  void set_prob_neuhad(size_t n, float v) {
    fTableData[n].prob_neuhad = v;
  }
  float get_prob_neuhad(size_t n) const {
    return fTableData[n].prob_neuhad;
  }
  void set_chi2(size_t n, float v) {
    fTableData[n].chi2 = v;
  }
  float get_chi2(size_t n) const {
    return fTableData[n].chi2;
  }
  void set_nsh(size_t n, short v) {
    fTableData[n].nsh = v;
  }
  short get_nsh(size_t n) const {
    return fTableData[n].nsh;
  }
  void set_chi2_sh(size_t n, float v) {
    fTableData[n].chi2_sh = v;
  }
  float get_chi2_sh(size_t n) const {
    return fTableData[n].chi2_sh;
  }
  void set_prob_photon_sh(size_t n, float v) {
    fTableData[n].prob_photon_sh = v;
  }
  float get_prob_photon_sh(size_t n) const {
    return fTableData[n].prob_photon_sh;
  }
  void set_e_sh(size_t d0, size_t n, float v) {
    fTableData[n].e_sh[d0] = v;
  }
  float get_e_sh(size_t d0, size_t n) const {
    return fTableData[n].e_sh[d0];
  }
  void set_ecorr_sh(size_t d0, size_t n, float v) {
    fTableData[n].ecorr_sh[d0] = v;
  }
  float get_ecorr_sh(size_t d0, size_t n) const {
    return fTableData[n].ecorr_sh[d0];
  }
  void set_de_sh(size_t d0, size_t n, float v) {
    fTableData[n].de_sh[d0] = v;
  }
  float get_de_sh(size_t d0, size_t n) const {
    return fTableData[n].de_sh[d0];
  }
  void set_xyz_sh(size_t d0, size_t d1, size_t n, float v) {
    fTableData[n].xyz_sh[d0][d1] = v;
  }
  float get_xyz_sh(size_t d0, size_t d1, size_t n) const {
    return fTableData[n].xyz_sh[d0][d1];
  }
  void set_dxyz_sh(size_t d0, size_t d1, size_t n, float v) {
    fTableData[n].dxyz_sh[d0][d1] = v;
  }
  float get_dxyz_sh(size_t d0, size_t d1, size_t n) const {
    return fTableData[n].dxyz_sh[d0][d1];
  }
  void set_theta(size_t n, float v) {
    fTableData[n].theta = v;
  }
  float get_theta(size_t n) const {
    return fTableData[n].theta;
  }
  void set_phi(size_t n, float v) {
    fTableData[n].phi = v;
  }
  float get_phi(size_t n) const {
    return fTableData[n].phi;
  }
  void set_unitv(size_t d0, size_t n, float v) {
    fTableData[n].unitv[d0] = v;
  }
  float get_unitv(size_t d0, size_t n) const {
    return fTableData[n].unitv[d0];
  }
  void set_ind(size_t d0, size_t n, short v) {
    fTableData[n].ind[d0] = v;
  }
  short get_ind(size_t d0, size_t n) const {
    return fTableData[n].ind[d0];
  }
  void set_twrhit(size_t n, short v) {
    fTableData[n].twrhit = v;
  }
  short get_twrhit(size_t n) const {
    return fTableData[n].twrhit;
  }
  void set_tofmin(size_t n, float v) {
    fTableData[n].tofmin = v;
  }
  float get_tofmin(size_t n) const {
    return fTableData[n].tofmin;
  }
  void set_etofmin(size_t n, float v) {
    fTableData[n].etofmin = v;
  }
  float get_etofmin(size_t n) const {
    return fTableData[n].etofmin;
  }
  void set_tofmincorr(size_t n, float v) {
    fTableData[n].tofmincorr = v;
  }
  float get_tofmincorr(size_t n) const {
    return fTableData[n].tofmincorr;
  }
  void set_tofmax(size_t n, float v) {
    fTableData[n].tofmax = v;
  }
  float get_tofmax(size_t n) const {
    return fTableData[n].tofmax;
  }
  void set_etofmax(size_t n, float v) {
    fTableData[n].etofmax = v;
  }
  float get_etofmax(size_t n) const {
    return fTableData[n].etofmax;
  }
  void set_tofmaxcorr(size_t n, float v) {
    fTableData[n].tofmaxcorr = v;
  }
  float get_tofmaxcorr(size_t n) const {
    return fTableData[n].tofmaxcorr;
  }
  void set_tofmean(size_t n, float v) {
    fTableData[n].tofmean = v;
  }
  float get_tofmean(size_t n) const {
    return fTableData[n].tofmean;
  }
  void set_disp(size_t d0, size_t n, float v) {
    fTableData[n].disp[d0] = v;
  }
  float get_disp(size_t d0, size_t n) const {
    return fTableData[n].disp[d0];
  }
  void set_padisp(size_t d0, size_t n, float v) {
    fTableData[n].padisp[d0] = v;
  }
  float get_padisp(size_t d0, size_t n) const {
    return fTableData[n].padisp[d0];
  }
  void set_charged(size_t n, int v) {
    fTableData[n].charged = v;
  }
  int get_charged(size_t n) const {
    return fTableData[n].charged;
  }
  void set_pc3proj(size_t d0, size_t n, float v) {
    fTableData[n].pc3proj[d0] = v;
  }
  float get_pc3proj(size_t d0, size_t n) const {
    return fTableData[n].pc3proj[d0];
  }
  void set_partesum(size_t d0, size_t n, float v) {
    fTableData[n].partesum[d0] = v;
  }
  float get_partesum(size_t d0, size_t n) const {
    return fTableData[n].partesum[d0];
  }
  void set_twrlist(size_t d0, size_t n, int v) {
    fTableData[n].twrlist[d0] = v;
  }
  int get_twrlist(size_t d0, size_t n) const {
    return fTableData[n].twrlist[d0];
  }
  void set_chglist(size_t d0, size_t n, int v) {
    fTableData[n].chglist[d0] = v;
  }
  int get_chglist(size_t d0, size_t n) const {
    return fTableData[n].chglist[d0];
  }

private:
  DEMCCLUSTEREXT_ST* fTableData;

  ClassDef(dEmcClusterExtWrapper,1)
};
#endif /*__DEMCCLUSTEREXTWRAPPER_H__*/
