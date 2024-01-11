#ifndef __DEMCRAWDATAWRAPPER_H__
#define __DEMCRAWDATAWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcRawData.h"
class dEmcRawDataWrapper: public PHTable
{
public:
  dEmcRawDataWrapper(const char* name = "dEmcRawData", const size_t& max_rows = 1);
//  dEmcRawDataWrapper(const dEmcRawDataWrapper& source);
//  dEmcRawDataWrapper& operator=(const dEmcRawDataWrapper& source);

  ~dEmcRawDataWrapper();

  void* RawTableData();
  DEMCRAWDATA_ST* TableData();

  DEMCRAWDATA_ST& operator[](const size_t& row);
  const DEMCRAWDATA_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_evno(size_t n, int v) {
    fTableData[n].evno = v;
  }
  int get_evno(size_t n) const {
    return fTableData[n].evno;
  }
  void set_hwkey(size_t n, int v) {
    fTableData[n].hwkey = v;
  }
  int get_hwkey(size_t n) const {
    return fTableData[n].hwkey;
  }
  void set_swkey(size_t n, int v) {
    fTableData[n].swkey = v;
  }
  int get_swkey(size_t n) const {
    return fTableData[n].swkey;
  }
  void set_type(size_t n, short v) {
    fTableData[n].type = v;
  }
  short get_type(size_t n) const {
    return fTableData[n].type;
  }
  void set_adclopre(size_t n, short v) {
    fTableData[n].adclopre = v;
  }
  short get_adclopre(size_t n) const {
    return fTableData[n].adclopre;
  }
  void set_adclopost(size_t n, short v) {
    fTableData[n].adclopost = v;
  }
  short get_adclopost(size_t n) const {
    return fTableData[n].adclopost;
  }
  void set_adchipre(size_t n, short v) {
    fTableData[n].adchipre = v;
  }
  short get_adchipre(size_t n) const {
    return fTableData[n].adchipre;
  }
  void set_adchipost(size_t n, short v) {
    fTableData[n].adchipost = v;
  }
  short get_adchipost(size_t n) const {
    return fTableData[n].adchipost;
  }
  void set_tdc(size_t n, short v) {
    fTableData[n].tdc = v;
  }
  short get_tdc(size_t n) const {
    return fTableData[n].tdc;
  }

private:
  DEMCRAWDATA_ST* fTableData;

  ClassDef(dEmcRawDataWrapper,1)
};
#endif /*__DEMCRAWDATAWRAPPER_H__*/
