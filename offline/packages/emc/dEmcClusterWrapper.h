#ifndef __DEMCCLUSTERWRAPPER_H__
#define __DEMCCLUSTERWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcCluster.h"
class dEmcClusterWrapper: public PHTable
{
public:
  dEmcClusterWrapper(const char* name = "dEmcCluster", const size_t& max_rows = 1);
//  dEmcClusterWrapper(const dEmcClusterWrapper& source);
//  dEmcClusterWrapper& operator=(const dEmcClusterWrapper& source);

  ~dEmcClusterWrapper();

  void* RawTableData();
  DEMCCLUSTER_ST* TableData();

  DEMCCLUSTER_ST& operator[](const size_t& row);
  const DEMCCLUSTER_ST& operator[](const size_t& row) const;

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

private:
  DEMCCLUSTER_ST* fTableData;

  ClassDef(dEmcClusterWrapper,1)
};
#endif /*__DEMCCLUSTERWRAPPER_H__*/
