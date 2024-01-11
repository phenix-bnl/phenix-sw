#ifndef __DTOFEVALWRAPPER_H__
#define __DTOFEVALWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofEval.h"
class dTofEvalWrapper: public PHTable
{
public:
  dTofEvalWrapper(const char* name = "dTofEval", const size_t& max_rows = 1);
  dTofEvalWrapper(const dTofEvalWrapper& source);
  dTofEvalWrapper& operator=(const dTofEvalWrapper& source);

  ~dTofEvalWrapper();

  void* RawTableData();
  DTOFEVAL_ST* TableData();

  DTOFEVAL_ST& operator[](const size_t& row);
  const DTOFEVAL_ST& operator[](const size_t& row) const;
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
  void set_slatid(size_t n, short v) {
    fTableData[n].slatid = v;
  }
  short get_slatid(size_t n) const {
    return fTableData[n].slatid;
  }
  void set_sector(size_t n, short v) {
    fTableData[n].sector = v;
  }
  short get_sector(size_t n) const {
    return fTableData[n].sector;
  }
  void set_side(size_t n, short v) {
    fTableData[n].side = v;
  }
  short get_side(size_t n) const {
    return fTableData[n].side;
  }
  void set_panel(size_t n, short v) {
    fTableData[n].panel = v;
  }
  short get_panel(size_t n) const {
    return fTableData[n].panel;
  }
  void set_slat(size_t n, short v) {
    fTableData[n].slat = v;
  }
  short get_slat(size_t n) const {
    return fTableData[n].slat;
  }
  void set_rawid(size_t n, short v) {
    fTableData[n].rawid = v;
  }
  short get_rawid(size_t n) const {
    return fTableData[n].rawid;
  }
  void set_recid(size_t n, short v) {
    fTableData[n].recid = v;
  }
  short get_recid(size_t n) const {
    return fTableData[n].recid;
  }
  void set_gdigiid(size_t n, short v) {
    fTableData[n].gdigiid = v;
  }
  short get_gdigiid(size_t n) const {
    return fTableData[n].gdigiid;
  }
  void set_nslathit(size_t n, short v) {
    fTableData[n].nslathit = v;
  }
  short get_nslathit(size_t n) const {
    return fTableData[n].nslathit;
  }
  void set_hits_seq(size_t n, short v) {
    fTableData[n].hits_seq = v;
  }
  short get_hits_seq(size_t n) const {
    return fTableData[n].hits_seq;
  }
  void set_nrectrack(size_t n, short v) {
    fTableData[n].nrectrack = v;
  }
  short get_nrectrack(size_t n) const {
    return fTableData[n].nrectrack;
  }
  void set_rawqvc(size_t d0, size_t n, short v) {
    fTableData[n].rawqvc[d0] = v;
  }
  short get_rawqvc(size_t d0, size_t n) const {
    return fTableData[n].rawqvc[d0];
  }
  void set_rawtvc(size_t d0, size_t n, short v) {
    fTableData[n].rawtvc[d0] = v;
  }
  short get_rawtvc(size_t d0, size_t n) const {
    return fTableData[n].rawtvc[d0];
  }
  void set_dtof(size_t n, float v) {
    fTableData[n].dtof = v;
  }
  float get_dtof(size_t n) const {
    return fTableData[n].dtof;
  }
  void set_rectof(size_t n, float v) {
    fTableData[n].rectof = v;
  }
  float get_rectof(size_t n) const {
    return fTableData[n].rectof;
  }
  void set_mctof(size_t n, float v) {
    fTableData[n].mctof = v;
  }
  float get_mctof(size_t n) const {
    return fTableData[n].mctof;
  }
  void set_deloss(size_t n, float v) {
    fTableData[n].deloss = v;
  }
  float get_deloss(size_t n) const {
    return fTableData[n].deloss;
  }
  void set_receloss(size_t n, float v) {
    fTableData[n].receloss = v;
  }
  float get_receloss(size_t n) const {
    return fTableData[n].receloss;
  }
  void set_mceloss(size_t n, float v) {
    fTableData[n].mceloss = v;
  }
  float get_mceloss(size_t n) const {
    return fTableData[n].mceloss;
  }
  void set_drphi(size_t n, float v) {
    fTableData[n].drphi = v;
  }
  float get_drphi(size_t n) const {
    return fTableData[n].drphi;
  }
  void set_dz(size_t n, float v) {
    fTableData[n].dz = v;
  }
  float get_dz(size_t n) const {
    return fTableData[n].dz;
  }
  void set_recpos(size_t d0, size_t n, float v) {
    fTableData[n].recpos[d0] = v;
  }
  float get_recpos(size_t d0, size_t n) const {
    return fTableData[n].recpos[d0];
  }
  void set_mcpos(size_t d0, size_t n, float v) {
    fTableData[n].mcpos[d0] = v;
  }
  float get_mcpos(size_t d0, size_t n) const {
    return fTableData[n].mcpos[d0];
  }
  void set_mcpid(size_t n, short v) {
    fTableData[n].mcpid = v;
  }
  short get_mcpid(size_t n) const {
    return fTableData[n].mcpid;
  }
  void set_r_vertex(size_t n, float v) {
    fTableData[n].r_vertex = v;
  }
  float get_r_vertex(size_t n) const {
    return fTableData[n].r_vertex;
  }
  void set_z_vertex(size_t n, float v) {
    fTableData[n].z_vertex = v;
  }
  float get_z_vertex(size_t n) const {
    return fTableData[n].z_vertex;
  }
  void set_phi_vertex(size_t n, float v) {
    fTableData[n].phi_vertex = v;
  }
  float get_phi_vertex(size_t n) const {
    return fTableData[n].phi_vertex;
  }
  void set_vertex(size_t d0, size_t n, float v) {
    fTableData[n].vertex[d0] = v;
  }
  float get_vertex(size_t d0, size_t n) const {
    return fTableData[n].vertex[d0];
  }
  void set_p_vertex(size_t n, float v) {
    fTableData[n].p_vertex = v;
  }
  float get_p_vertex(size_t n) const {
    return fTableData[n].p_vertex;
  }
  void set_ptheta(size_t n, float v) {
    fTableData[n].ptheta = v;
  }
  float get_ptheta(size_t n) const {
    return fTableData[n].ptheta;
  }
  void set_pphi(size_t n, float v) {
    fTableData[n].pphi = v;
  }
  float get_pphi(size_t n) const {
    return fTableData[n].pphi;
  }
  void set_idparent(size_t n, short v) {
    fTableData[n].idparent = v;
  }
  short get_idparent(size_t n) const {
    return fTableData[n].idparent;
  }
  void set_r_verorg(size_t n, float v) {
    fTableData[n].r_verorg = v;
  }
  float get_r_verorg(size_t n) const {
    return fTableData[n].r_verorg;
  }
  void set_z_verorg(size_t n, float v) {
    fTableData[n].z_verorg = v;
  }
  float get_z_verorg(size_t n) const {
    return fTableData[n].z_verorg;
  }
  void set_phi_verorg(size_t n, float v) {
    fTableData[n].phi_verorg = v;
  }
  float get_phi_verorg(size_t n) const {
    return fTableData[n].phi_verorg;
  }
  void set_p_verorg(size_t n, float v) {
    fTableData[n].p_verorg = v;
  }
  float get_p_verorg(size_t n) const {
    return fTableData[n].p_verorg;
  }
  void set_pthetaorg(size_t n, float v) {
    fTableData[n].pthetaorg = v;
  }
  float get_pthetaorg(size_t n) const {
    return fTableData[n].pthetaorg;
  }
  void set_pphiorg(size_t n, float v) {
    fTableData[n].pphiorg = v;
  }
  float get_pphiorg(size_t n) const {
    return fTableData[n].pphiorg;
  }
  void set_idorigin(size_t n, short v) {
    fTableData[n].idorigin = v;
  }
  short get_idorigin(size_t n) const {
    return fTableData[n].idorigin;
  }

private:
  DTOFEVAL_ST* fTableData;

  ClassDef(dTofEvalWrapper,1)
};
#endif /*__DTOFEVALWRAPPER_H__*/
