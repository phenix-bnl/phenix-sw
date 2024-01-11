#ifndef __DTOFCALWRAPPER_H__
#define __DTOFCALWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofCal.h"
class dTofCalWrapper: public PHTable
{
public:
  dTofCalWrapper(const char* name = "dTofCal", const size_t& max_rows = 1);
  dTofCalWrapper(const dTofCalWrapper& source);
  dTofCalWrapper& operator=(const dTofCalWrapper& source);

  ~dTofCalWrapper();

  void* RawTableData();
  DTOFCAL_ST* TableData();

  DTOFCAL_ST& operator[](const size_t& row);
  const DTOFCAL_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_slatid(size_t n, short v) {
    fTableData[n].slatid = v;
  }
  short get_slatid(size_t n) const {
    return fTableData[n].slatid;
  }
  void set_qvc_corr(size_t d0, size_t n, float v) {
    fTableData[n].qvc_corr[d0] = v;
  }
  float get_qvc_corr(size_t d0, size_t n) const {
    return fTableData[n].qvc_corr[d0];
  }
  void set_qvc_corrlsr(size_t d0, size_t n, float v) {
    fTableData[n].qvc_corrlsr[d0] = v;
  }
  float get_qvc_corrlsr(size_t d0, size_t n) const {
    return fTableData[n].qvc_corrlsr[d0];
  }
  void set_eloss_conv(size_t n, float v) {
    fTableData[n].eloss_conv = v;
  }
  float get_eloss_conv(size_t n) const {
    return fTableData[n].eloss_conv;
  }
  void set_eloss_mip(size_t n, float v) {
    fTableData[n].eloss_mip = v;
  }
  float get_eloss_mip(size_t n) const {
    return fTableData[n].eloss_mip;
  }
  void set_tvc_conv(size_t d0, size_t n, float v) {
    fTableData[n].tvc_conv[d0] = v;
  }
  float get_tvc_conv(size_t d0, size_t n) const {
    return fTableData[n].tvc_conv[d0];
  }
  void set_tvc_ped(size_t d0, size_t n, float v) {
    fTableData[n].tvc_ped[d0] = v;
  }
  float get_tvc_ped(size_t d0, size_t n) const {
    return fTableData[n].tvc_ped[d0];
  }
  void set_t0(size_t d0, size_t n, float v) {
    fTableData[n].t0[d0] = v;
  }
  float get_t0(size_t d0, size_t n) const {
    return fTableData[n].t0[d0];
  }
  void set_t0_lsr(size_t d0, size_t n, float v) {
    fTableData[n].t0_lsr[d0] = v;
  }
  float get_t0_lsr(size_t d0, size_t n) const {
    return fTableData[n].t0_lsr[d0];
  }
  void set_slew_a(size_t d0, size_t n, float v) {
    fTableData[n].slew_a[d0] = v;
  }
  float get_slew_a(size_t d0, size_t n) const {
    return fTableData[n].slew_a[d0];
  }
  void set_slew_b(size_t d0, size_t n, float v) {
    fTableData[n].slew_b[d0] = v;
  }
  float get_slew_b(size_t d0, size_t n) const {
    return fTableData[n].slew_b[d0];
  }
  void set_scint_vlight(size_t n, float v) {
    fTableData[n].scint_vlight = v;
  }
  float get_scint_vlight(size_t n) const {
    return fTableData[n].scint_vlight;
  }
  void set_scint_attenu(size_t n, float v) {
    fTableData[n].scint_attenu = v;
  }
  float get_scint_attenu(size_t n) const {
    return fTableData[n].scint_attenu;
  }

private:
  DTOFCAL_ST* fTableData;

  ClassDef(dTofCalWrapper,1)
};
#endif /*__DTOFCALWRAPPER_H__*/
