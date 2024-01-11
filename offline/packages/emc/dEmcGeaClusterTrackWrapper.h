#ifndef __DEMCGEACLUSTERTRACKWRAPPER_H__
#define __DEMCGEACLUSTERTRACKWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcGeaClusterTrack.h"
class dEmcGeaClusterTrackWrapper: public PHTable
{
public:
  dEmcGeaClusterTrackWrapper(const char* name = "dEmcGeaClusterTrack", const size_t& max_rows = 1);
//  dEmcGeaClusterTrackWrapper(const dEmcGeaClusterTrackWrapper& source);
//  dEmcGeaClusterTrackWrapper& operator=(const dEmcGeaClusterTrackWrapper& source);

  ~dEmcGeaClusterTrackWrapper();

  void* RawTableData();
  DEMCGEACLUSTERTRACK_ST* TableData();

  DEMCGEACLUSTERTRACK_ST& operator[](const size_t& row);
  const DEMCGEACLUSTERTRACK_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, int v) {
    fTableData[n].id = v;
  }
  int get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_clusid(size_t n, int v) {
    fTableData[n].clusid = v;
  }
  int get_clusid(size_t n) const {
    return fTableData[n].clusid;
  }
  void set_evno(size_t n, int v) {
    fTableData[n].evno = v;
  }
  int get_evno(size_t n) const {
    return fTableData[n].evno;
  }
  void set_keycent(size_t n, int v) {
    fTableData[n].keycent = v;
  }
  int get_keycent(size_t n) const {
    return fTableData[n].keycent;
  }
  void set_input(size_t n, int v) {
    fTableData[n].input = v;
  }
  int get_input(size_t n) const {
    return fTableData[n].input;
  }
  void set_type(size_t n, int v) {
    fTableData[n].type = v;
  }
  int get_type(size_t n) const {
    return fTableData[n].type;
  }
  void set_arm(size_t n, int v) {
    fTableData[n].arm = v;
  }
  int get_arm(size_t n) const {
    return fTableData[n].arm;
  }
  void set_sector(size_t n, int v) {
    fTableData[n].sector = v;
  }
  int get_sector(size_t n) const {
    return fTableData[n].sector;
  }
  void set_trkno(size_t d0, size_t n, int v) {
    fTableData[n].trkno[d0] = v;
  }
  int get_trkno(size_t d0, size_t n) const {
    return fTableData[n].trkno[d0];
  }
  void set_tracktwrhit(size_t d0, size_t n, int v) {
    fTableData[n].tracktwrhit[d0] = v;
  }
  int get_tracktwrhit(size_t d0, size_t n) const {
    return fTableData[n].tracktwrhit[d0];
  }
  void set_edep_nom(size_t d0, size_t n, float v) {
    fTableData[n].edep_nom[d0] = v;
  }
  float get_edep_nom(size_t d0, size_t n) const {
    return fTableData[n].edep_nom[d0];
  }
  void set_pid(size_t d0, size_t n, float v) {
    fTableData[n].pid[d0] = v;
  }
  float get_pid(size_t d0, size_t n) const {
    return fTableData[n].pid[d0];
  }
  void set_ptot(size_t d0, size_t n, float v) {
    fTableData[n].ptot[d0] = v;
  }
  float get_ptot(size_t d0, size_t n) const {
    return fTableData[n].ptot[d0];
  }
  void set_vertex(size_t d0, size_t d1, size_t n, float v) {
    fTableData[n].vertex[d0][d1] = v;
  }
  float get_vertex(size_t d0, size_t d1, size_t n) const {
    return fTableData[n].vertex[d0][d1];
  }
  void set_ancestry(size_t d0, size_t n, float v) {
    fTableData[n].ancestry[d0] = v;
  }
  float get_ancestry(size_t d0, size_t n) const {
    return fTableData[n].ancestry[d0];
  }
  void set_xyz(size_t d0, size_t d1, size_t n, float v) {
    fTableData[n].xyz[d0][d1] = v;
  }
  float get_xyz(size_t d0, size_t d1, size_t n) const {
    return fTableData[n].xyz[d0][d1];
  }
  void set_edep(size_t d0, size_t n, float v) {
    fTableData[n].edep[d0] = v;
  }
  float get_edep(size_t d0, size_t n) const {
    return fTableData[n].edep[d0];
  }
  void set_efrac(size_t d0, size_t n, float v) {
    fTableData[n].efrac[d0] = v;
  }
  float get_efrac(size_t d0, size_t n) const {
    return fTableData[n].efrac[d0];
  }
  void set_measxyz(size_t d0, size_t n, float v) {
    fTableData[n].measxyz[d0] = v;
  }
  float get_measxyz(size_t d0, size_t n) const {
    return fTableData[n].measxyz[d0];
  }
  void set_mease(size_t n, float v) {
    fTableData[n].mease = v;
  }
  float get_mease(size_t n) const {
    return fTableData[n].mease;
  }
  void set_ecore(size_t n, float v) {
    fTableData[n].ecore = v;
  }
  float get_ecore(size_t n) const {
    return fTableData[n].ecore;
  }
  void set_tof(size_t n, float v) {
    fTableData[n].tof = v;
  }
  float get_tof(size_t n) const {
    return fTableData[n].tof;
  }
  void set_etof(size_t n, float v) {
    fTableData[n].etof = v;
  }
  float get_etof(size_t n) const {
    return fTableData[n].etof;
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
  void set_twrhit(size_t n, int v) {
    fTableData[n].twrhit = v;
  }
  int get_twrhit(size_t n) const {
    return fTableData[n].twrhit;
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
  void set_partesum(size_t d0, size_t n, float v) {
    fTableData[n].partesum[d0] = v;
  }
  float get_partesum(size_t d0, size_t n) const {
    return fTableData[n].partesum[d0];
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
  void set_chglist(size_t d0, size_t n, float v) {
    fTableData[n].chglist[d0] = v;
  }
  float get_chglist(size_t d0, size_t n) const {
    return fTableData[n].chglist[d0];
  }

private:
  DEMCGEACLUSTERTRACK_ST* fTableData;

  ClassDef(dEmcGeaClusterTrackWrapper,1)
};
#endif /*__DEMCGEACLUSTERTRACKWRAPPER_H__*/
