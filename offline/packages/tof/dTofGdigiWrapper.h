#ifndef __DTOFGDIGIWRAPPER_H__
#define __DTOFGDIGIWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofGdigi.h"
class dTofGdigiWrapper: public PHTable
{
public:
  dTofGdigiWrapper(const char* name = "dTofGdigi", const size_t& max_rows = 1);
  dTofGdigiWrapper(const dTofGdigiWrapper& source);
  dTofGdigiWrapper& operator=(const dTofGdigiWrapper& source);

  ~dTofGdigiWrapper();

  void* RawTableData();
  DTOFGDIGI_ST* TableData();

  DTOFGDIGI_ST& operator[](const size_t& row);
  const DTOFGDIGI_ST& operator[](const size_t& row) const;
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
  void set_panel(size_t n, short v) {
    fTableData[n].panel = v;
  }
  short get_panel(size_t n) const {
    return fTableData[n].panel;
  }
  void set_column(size_t n, short v) {
    fTableData[n].column = v;
  }
  short get_column(size_t n) const {
    return fTableData[n].column;
  }
  void set_pslat(size_t n, short v) {
    fTableData[n].pslat = v;
  }
  short get_pslat(size_t n) const {
    return fTableData[n].pslat;
  }
  void set_slat_seq(size_t n, short v) {
    fTableData[n].slat_seq = v;
  }
  short get_slat_seq(size_t n) const {
    return fTableData[n].slat_seq;
  }
  void set_mctrack(size_t n, int v) {
    fTableData[n].mctrack = v;
  }
  int get_mctrack(size_t n) const {
    return fTableData[n].mctrack;
  }
  void set_partl(size_t n, short v) {
    fTableData[n].partl = v;
  }
  short get_partl(size_t n) const {
    return fTableData[n].partl;
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
  void set_pos_m(size_t d0, size_t n, float v) {
    fTableData[n].pos_m[d0] = v;
  }
  float get_pos_m(size_t d0, size_t n) const {
    return fTableData[n].pos_m[d0];
  }
  void set_pos_hit_slat(size_t n, float v) {
    fTableData[n].pos_hit_slat = v;
  }
  float get_pos_hit_slat(size_t n) const {
    return fTableData[n].pos_hit_slat;
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
  void set_p_m(size_t d0, size_t n, float v) {
    fTableData[n].p_m[d0] = v;
  }
  float get_p_m(size_t d0, size_t n) const {
    return fTableData[n].p_m[d0];
  }
  void set_path(size_t n, float v) {
    fTableData[n].path = v;
  }
  float get_path(size_t n) const {
    return fTableData[n].path;
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

private:
  DTOFGDIGI_ST* fTableData;

  ClassDef(dTofGdigiWrapper,1)
};
#endif /*__DTOFGDIGIWRAPPER_H__*/
