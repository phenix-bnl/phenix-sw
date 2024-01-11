#ifndef __DCRKFEMWRAPPER_H__
#define __DCRKFEMWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkFEM.h"
class dCrkFEMWrapper: public PHTable
{
public:
  dCrkFEMWrapper(const char* name = "dCrkFEM", const size_t& max_rows = 1);
//  dCrkFEMWrapper(const dCrkFEMWrapper& source);
//  dCrkFEMWrapper& operator=(const dCrkFEMWrapper& source);

  ~dCrkFEMWrapper();

  void* RawTableData();
  DCRKFEM_ST* TableData();

  DCRKFEM_ST& operator[](const size_t& row);
  const DCRKFEM_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_DAV1(size_t n, unsigned short v) {
    fTableData[n].DAV1 = v;
  }
  unsigned short get_DAV1(size_t n) const {
    return fTableData[n].DAV1;
  }
  void set_detid(size_t n, unsigned short v) {
    fTableData[n].detid = v;
  }
  unsigned short get_detid(size_t n) const {
    return fTableData[n].detid;
  }
  void set_evno(size_t n, unsigned short v) {
    fTableData[n].evno = v;
  }
  unsigned short get_evno(size_t n) const {
    return fTableData[n].evno;
  }
  void set_module(size_t n, unsigned short v) {
    fTableData[n].module = v;
  }
  unsigned short get_module(size_t n) const {
    return fTableData[n].module;
  }
  void set_flag(size_t n, unsigned short v) {
    fTableData[n].flag = v;
  }
  unsigned short get_flag(size_t n) const {
    return fTableData[n].flag;
  }
  void set_clock(size_t n, unsigned short v) {
    fTableData[n].clock = v;
  }
  unsigned short get_clock(size_t n) const {
    return fTableData[n].clock;
  }
  void set_tac_cell(size_t n, unsigned short v) {
    fTableData[n].tac_cell = v;
  }
  unsigned short get_tac_cell(size_t n) const {
    return fTableData[n].tac_cell;
  }
  void set_pre_cell(size_t n, unsigned short v) {
    fTableData[n].pre_cell = v;
  }
  unsigned short get_pre_cell(size_t n) const {
    return fTableData[n].pre_cell;
  }
  void set_post_cell(size_t n, unsigned short v) {
    fTableData[n].post_cell = v;
  }
  unsigned short get_post_cell(size_t n) const {
    return fTableData[n].post_cell;
  }
  void set_data(size_t d0, size_t n, unsigned short v) {
    fTableData[n].data[d0] = v;
  }
  unsigned short get_data(size_t d0, size_t n) const {
    return fTableData[n].data[d0];
  }
  void set_usr(size_t d0, size_t n, unsigned short v) {
    fTableData[n].usr[d0] = v;
  }
  unsigned short get_usr(size_t d0, size_t n) const {
    return fTableData[n].usr[d0];
  }
  void set_parity(size_t n, unsigned short v) {
    fTableData[n].parity = v;
  }
  unsigned short get_parity(size_t n) const {
    return fTableData[n].parity;
  }
  void set_CAV2(size_t n, unsigned short v) {
    fTableData[n].CAV2 = v;
  }
  unsigned short get_CAV2(size_t n) const {
    return fTableData[n].CAV2;
  }

private:
  DCRKFEM_ST* fTableData;

  ClassDef(dCrkFEMWrapper,1)
};
#endif /*__DCRKFEMWRAPPER_H__*/
