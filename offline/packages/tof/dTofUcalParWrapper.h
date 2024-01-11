#ifndef __DTOFUCALPARWRAPPER_H__
#define __DTOFUCALPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofUcalPar.h"
class dTofUcalParWrapper: public PHTable
{
public:
  dTofUcalParWrapper(const char* name = "dTofUcalPar", const size_t& max_rows = 1);
  dTofUcalParWrapper(const dTofUcalParWrapper& source);
  dTofUcalParWrapper& operator=(const dTofUcalParWrapper& source);

  ~dTofUcalParWrapper();

  void* RawTableData();
  DTOFUCALPAR_ST* TableData();

  DTOFUCALPAR_ST& operator[](const size_t& row);
  const DTOFUCALPAR_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_option(size_t n, short v) {
    fTableData[n].option = v;
  }
  short get_option(size_t n) const {
    return fTableData[n].option;
  }
  void set_qvc_chgain(size_t d0, size_t n, float v) {
    fTableData[n].qvc_chgain[d0] = v;
  }
  float get_qvc_chgain(size_t d0, size_t n) const {
    return fTableData[n].qvc_chgain[d0];
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
  void set_tof_sigma(size_t n, float v) {
    fTableData[n].tof_sigma = v;
  }
  float get_tof_sigma(size_t n) const {
    return fTableData[n].tof_sigma;
  }
  void set_datafile(size_t d0, size_t n, char v) {
    fTableData[n].datafile[d0] = v;
  }
  char get_datafile(size_t d0, size_t n) const {
    return fTableData[n].datafile[d0];
  }

private:
  DTOFUCALPAR_ST* fTableData;

  ClassDef(dTofUcalParWrapper,1)
};
#endif /*__DTOFUCALPARWRAPPER_H__*/
