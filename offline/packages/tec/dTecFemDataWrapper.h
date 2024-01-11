#ifndef __DTECFEMDATAWRAPPER_H__
#define __DTECFEMDATAWRAPPER_H__

#include <cstddef>
#include "PHTable.hh"
#include "dTecFemData.h"

class dTecFemDataWrapper: public PHTable
{
public:
  dTecFemDataWrapper(const char* name = "dTecFemData", const size_t& max_rows = 1);
//  dTecFemDataWrapper(const dTecFemDataWrapper& source);
//  dTecFemDataWrapper& operator=(const dTecFemDataWrapper& source);

  ~dTecFemDataWrapper();

  void* RawTableData();
  DTECFEMDATA_ST* TableData();

  DTECFEMDATA_ST& operator[](const size_t& row);
  const DTECFEMDATA_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_crate(size_t n, short v) {
    fTableData[n].crate = v;
  }
  short get_crate(size_t n) const {
    return fTableData[n].crate;
  }
  void set_slot(size_t n, short v) {
    fTableData[n].slot = v;
  }
  short get_slot(size_t n) const {
    return fTableData[n].slot;
  }
  void set_psadd(size_t n, short v) {
    fTableData[n].psadd = v;
  }
  short get_psadd(size_t n) const {
    return fTableData[n].psadd;
  }
  void set_ichan(size_t n, short v) {
    fTableData[n].ichan = v;
  }
  short get_ichan(size_t n) const {
    return fTableData[n].ichan;
  }
  void set_amplitude(size_t d0, size_t n, short v) {
    fTableData[n].amplitude[d0] = v;
  }
  short get_amplitude(size_t d0, size_t n) const {
    return fTableData[n].amplitude[d0];
  }

private:
  DTECFEMDATA_ST* fTableData;

  ClassDef(dTecFemDataWrapper,1)
};
#endif /*__DTECFEMDATAWRAPPER_H__*/
