#ifndef __DEMCFEMDATAWRAPPER_H__
#define __DEMCFEMDATAWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcFEMData.h"
class dEmcFEMDataWrapper: public PHTable
{
public:
  dEmcFEMDataWrapper(const char* name = "dEmcFEMData", const size_t& max_rows = 1);
//  dEmcFEMDataWrapper(const dEmcFEMDataWrapper& source);
//  dEmcFEMDataWrapper& operator=(const dEmcFEMDataWrapper& source);

  ~dEmcFEMDataWrapper();

  void* RawTableData();
  DEMCFEMDATA_ST* TableData();

  DEMCFEMDATA_ST& operator[](const size_t& row);
  const DEMCFEMDATA_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_header(size_t n, short v) {
    fTableData[n].header = v;
  }
  short get_header(size_t n) const {
    return fTableData[n].header;
  }
  void set_detid(size_t n, short v) {
    fTableData[n].detid = v;
  }
  short get_detid(size_t n) const {
    return fTableData[n].detid;
  }
  void set_evno(size_t n, short v) {
    fTableData[n].evno = v;
  }
  short get_evno(size_t n) const {
    return fTableData[n].evno;
  }
  void set_module(size_t n, short v) {
    fTableData[n].module = v;
  }
  short get_module(size_t n) const {
    return fTableData[n].module;
  }
  void set_flag(size_t n, short v) {
    fTableData[n].flag = v;
  }
  short get_flag(size_t n) const {
    return fTableData[n].flag;
  }
  void set_clock(size_t n, short v) {
    fTableData[n].clock = v;
  }
  short get_clock(size_t n) const {
    return fTableData[n].clock;
  }
  void set_timecell(size_t n, short v) {
    fTableData[n].timecell = v;
  }
  short get_timecell(size_t n) const {
    return fTableData[n].timecell;
  }
  void set_precell(size_t n, short v) {
    fTableData[n].precell = v;
  }
  short get_precell(size_t n) const {
    return fTableData[n].precell;
  }
  void set_postcell(size_t n, short v) {
    fTableData[n].postcell = v;
  }
  short get_postcell(size_t n) const {
    return fTableData[n].postcell;
  }
  void set_data(size_t d0, size_t n, short v) {
    fTableData[n].data[d0] = v;
  }
  short get_data(size_t d0, size_t n) const {
    return fTableData[n].data[d0];
  }
  void set_userword(size_t d0, size_t n, short v) {
    fTableData[n].userword[d0] = v;
  }
  short get_userword(size_t d0, size_t n) const {
    return fTableData[n].userword[d0];
  }
  void set_longparity(size_t n, short v) {
    fTableData[n].longparity = v;
  }
  short get_longparity(size_t n) const {
    return fTableData[n].longparity;
  }
  void set_trailer(size_t n, short v) {
    fTableData[n].trailer = v;
  }
  short get_trailer(size_t n) const {
    return fTableData[n].trailer;
  }

private:
  DEMCFEMDATA_ST* fTableData;

  ClassDef(dEmcFEMDataWrapper,1)
};
#endif /*__DEMCFEMDATAWRAPPER_H__*/
