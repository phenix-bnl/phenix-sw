#ifndef __DTOFASSOCIATEWRAPPER_H__
#define __DTOFASSOCIATEWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofAssociate.h"
class dTofAssociateWrapper: public PHTable
{
public:
  dTofAssociateWrapper(const char* name = "dTofAssociate", const size_t& max_rows = 1);
  dTofAssociateWrapper(const dTofAssociateWrapper& source);
  dTofAssociateWrapper& operator=(const dTofAssociateWrapper& source);

  ~dTofAssociateWrapper();

  void* RawTableData();
  DTOFASSOCIATE_ST* TableData();

  DTOFASSOCIATE_ST& operator[](const size_t& row);
  const DTOFASSOCIATE_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_id_cgl(size_t n, short v) {
    fTableData[n].id_cgl = v;
  }
  short get_id_cgl(size_t n) const {
    return fTableData[n].id_cgl;
  }
  void set_id_cgt(size_t n, short v) {
    fTableData[n].id_cgt = v;
  }
  short get_id_cgt(size_t n) const {
    return fTableData[n].id_cgt;
  }
  void set_id_dch(size_t n, short v) {
    fTableData[n].id_dch = v;
  }
  short get_id_dch(size_t n) const {
    return fTableData[n].id_dch;
  }
  void set_id_tec(size_t n, short v) {
    fTableData[n].id_tec = v;
  }
  short get_id_tec(size_t n) const {
    return fTableData[n].id_tec;
  }
  void set_id_tof(size_t n, short v) {
    fTableData[n].id_tof = v;
  }
  short get_id_tof(size_t n) const {
    return fTableData[n].id_tof;
  }
  void set_asc_stat(size_t n, unsigned short v) {
    fTableData[n].asc_stat = v;
  }
  unsigned short get_asc_stat(size_t n) const {
    return fTableData[n].asc_stat;
  }
  void set_slatid(size_t n, short v) {
    fTableData[n].slatid = v;
  }
  short get_slatid(size_t n) const {
    return fTableData[n].slatid;
  }
  void set_tof(size_t n, float v) {
    fTableData[n].tof = v;
  }
  float get_tof(size_t n) const {
    return fTableData[n].tof;
  }
  void set_eloss(size_t n, float v) {
    fTableData[n].eloss = v;
  }
  float get_eloss(size_t n) const {
    return fTableData[n].eloss;
  }
  void set_xtof(size_t d0, size_t n, float v) {
    fTableData[n].xtof[d0] = v;
  }
  float get_xtof(size_t d0, size_t n) const {
    return fTableData[n].xtof[d0];
  }
  void set_xtrk(size_t d0, size_t n, float v) {
    fTableData[n].xtrk[d0] = v;
  }
  float get_xtrk(size_t d0, size_t n) const {
    return fTableData[n].xtrk[d0];
  }
  void set_vtrk(size_t d0, size_t n, float v) {
    fTableData[n].vtrk[d0] = v;
  }
  float get_vtrk(size_t d0, size_t n) const {
    return fTableData[n].vtrk[d0];
  }
  void set_tof_stat(size_t n, unsigned short v) {
    fTableData[n].tof_stat = v;
  }
  unsigned short get_tof_stat(size_t n) const {
    return fTableData[n].tof_stat;
  }
  void set_path(size_t n, float v) {
    fTableData[n].path = v;
  }
  float get_path(size_t n) const {
    return fTableData[n].path;
  }
  void set_beta(size_t n, float v) {
    fTableData[n].beta = v;
  }
  float get_beta(size_t n) const {
    return fTableData[n].beta;
  }
  void set_m2(size_t n, float v) {
    fTableData[n].m2 = v;
  }
  float get_m2(size_t n) const {
    return fTableData[n].m2;
  }
  void set_charge(size_t n, short v) {
    fTableData[n].charge = v;
  }
  short get_charge(size_t n) const {
    return fTableData[n].charge;
  }
  void set_pxyz(size_t d0, size_t n, float v) {
    fTableData[n].pxyz[d0] = v;
  }
  float get_pxyz(size_t d0, size_t n) const {
    return fTableData[n].pxyz[d0];
  }
  void set_dpxyz(size_t d0, size_t n, float v) {
    fTableData[n].dpxyz[d0] = v;
  }
  float get_dpxyz(size_t d0, size_t n) const {
    return fTableData[n].dpxyz[d0];
  }
  void set_zvertex(size_t n, float v) {
    fTableData[n].zvertex = v;
  }
  float get_zvertex(size_t n) const {
    return fTableData[n].zvertex;
  }
  void set_dzvertex(size_t n, float v) {
    fTableData[n].dzvertex = v;
  }
  float get_dzvertex(size_t n) const {
    return fTableData[n].dzvertex;
  }
  void set_phi(size_t n, float v) {
    fTableData[n].phi = v;
  }
  float get_phi(size_t n) const {
    return fTableData[n].phi;
  }
  void set_dphi(size_t n, float v) {
    fTableData[n].dphi = v;
  }
  float get_dphi(size_t n) const {
    return fTableData[n].dphi;
  }
  void set_theta(size_t n, float v) {
    fTableData[n].theta = v;
  }
  float get_theta(size_t n) const {
    return fTableData[n].theta;
  }
  void set_dtheta(size_t n, float v) {
    fTableData[n].dtheta = v;
  }
  float get_dtheta(size_t n) const {
    return fTableData[n].dtheta;
  }
  void set_quality(size_t n, float v) {
    fTableData[n].quality = v;
  }
  float get_quality(size_t n) const {
    return fTableData[n].quality;
  }
  void set_momrec_ndist(size_t d0, size_t n, float v) {
    fTableData[n].momrec_ndist[d0] = v;
  }
  float get_momrec_ndist(size_t d0, size_t n) const {
    return fTableData[n].momrec_ndist[d0];
  }

private:
  DTOFASSOCIATE_ST* fTableData;

  ClassDef(dTofAssociateWrapper,1)
};
#endif /*__DTOFASSOCIATEWRAPPER_H__*/
