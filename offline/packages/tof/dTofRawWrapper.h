#ifndef __DTOFRAWWRAPPER_H__
#define __DTOFRAWWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofRaw.h"
class dTofRawWrapper: public PHTable
{
public:
  dTofRawWrapper(const char* name = "dTofRaw", const size_t& max_rows = 1);
  dTofRawWrapper(const dTofRawWrapper& source);
  dTofRawWrapper& operator=(const dTofRawWrapper& source);

  ~dTofRawWrapper();

  void* RawTableData();
  DTOFRAW_ST* TableData();

  DTOFRAW_ST& operator[](const size_t& row);
  const DTOFRAW_ST& operator[](const size_t& row) const;
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
  void set_cell(size_t d0, size_t n, short v) {
    fTableData[n].cell[d0] = v;
  }
  short get_cell(size_t d0, size_t n) const {
    return fTableData[n].cell[d0];
  }
  void set_qvc(size_t d0, size_t n, short v) {
    fTableData[n].qvc[d0] = v;
  }
  short get_qvc(size_t d0, size_t n) const {
    return fTableData[n].qvc[d0];
  }
  void set_q1(size_t d0, size_t n, short v) {
    fTableData[n].q1[d0] = v;
  }
  short get_q1(size_t d0, size_t n) const {
    return fTableData[n].q1[d0];
  }
  void set_q2(size_t d0, size_t n, short v) {
    fTableData[n].q2[d0] = v;
  }
  short get_q2(size_t d0, size_t n) const {
    return fTableData[n].q2[d0];
  }
  void set_tvc(size_t d0, size_t n, short v) {
    fTableData[n].tvc[d0] = v;
  }
  short get_tvc(size_t d0, size_t n) const {
    return fTableData[n].tvc[d0];
  }
  void set_t3(size_t d0, size_t n, short v) {
    fTableData[n].t3[d0] = v;
  }
  short get_t3(size_t d0, size_t n) const {
    return fTableData[n].t3[d0];
  }
  void set_t4(size_t d0, size_t n, short v) {
    fTableData[n].t4[d0] = v;
  }
  short get_t4(size_t d0, size_t n) const {
    return fTableData[n].t4[d0];
  }

private:
  DTOFRAW_ST* fTableData;

  ClassDef(dTofRawWrapper,1)
};
#endif /*__DTOFRAWWRAPPER_H__*/
