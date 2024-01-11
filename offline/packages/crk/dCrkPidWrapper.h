#ifndef __DCRKPIDWRAPPER_H__
#define __DCRKPIDWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkPid.h"
class dCrkPidWrapper: public PHTable
{
public:
  dCrkPidWrapper(const char* name = "dCrkPid", const size_t& max_rows = 1);
//  dCrkPidWrapper(const dCrkPidWrapper& source);
//  dCrkPidWrapper& operator=(const dCrkPidWrapper& source);

  ~dCrkPidWrapper();

  void* RawTableData();
  DCRKPID_ST* TableData();

  DCRKPID_ST& operator[](const size_t& row);
  const DCRKPID_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_proj_id(size_t n, short v) {
    fTableData[n].proj_id = v;
  }
  short get_proj_id(size_t n) const {
    return fTableData[n].proj_id;
  }
  void set_faccept(size_t n, short v) {
    fTableData[n].faccept = v;
  }
  short get_faccept(size_t n) const {
    return fTableData[n].faccept;
  }
  void set_npmt(size_t n, short v) {
    fTableData[n].npmt = v;
  }
  short get_npmt(size_t n) const {
    return fTableData[n].npmt;
  }
  void set_npe(size_t n, float v) {
    fTableData[n].npe = v;
  }
  float get_npe(size_t n) const {
    return fTableData[n].npe;
  }
  void set_timing(size_t n, float v) {
    fTableData[n].timing = v;
  }
  float get_timing(size_t n) const {
    return fTableData[n].timing;
  }
  void set_chi2(size_t n, float v) {
    fTableData[n].chi2 = v;
  }
  float get_chi2(size_t n) const {
    return fTableData[n].chi2;
  }
  void set_rdisp(size_t n, float v) {
    fTableData[n].rdisp = v;
  }
  float get_rdisp(size_t n) const {
    return fTableData[n].rdisp;
  }
  void set_Lpath(size_t n, float v) {
    fTableData[n].Lpath = v;
  }
  float get_Lpath(size_t n) const {
    return fTableData[n].Lpath;
  }
  void set_xproj(size_t d0, size_t n, float v) {
    fTableData[n].xproj[d0] = v;
  }
  float get_xproj(size_t d0, size_t n) const {
    return fTableData[n].xproj[d0];
  }
  void set_chi2b(size_t n, float v) {
    fTableData[n].chi2b = v;
  }
  float get_chi2b(size_t n) const {
    return fTableData[n].chi2b;
  }
  void set_dt(size_t n, float v) {
    fTableData[n].dt = v;
  }
  float get_dt(size_t n) const {
    return fTableData[n].dt;
  }

private:
  DCRKPID_ST* fTableData;

  ClassDef(dCrkPidWrapper,1)
};
#endif /*__DCRKPIDWRAPPER_H__*/
