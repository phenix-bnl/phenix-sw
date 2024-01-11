#ifndef __DTOFFEMMAPWRAPPER_H__
#define __DTOFFEMMAPWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofFEMmap.h"
class dTofFEMmapWrapper: public PHTable
{
public:
  dTofFEMmapWrapper(const char* name = "dTofFEMmap", const size_t& max_rows = 1);
  dTofFEMmapWrapper(const dTofFEMmapWrapper& source);
  dTofFEMmapWrapper& operator=(const dTofFEMmapWrapper& source);

  ~dTofFEMmapWrapper();

  void* RawTableData();
  DTOFFEMMAP_ST* TableData();

  DTOFFEMMAP_ST& operator[](const size_t& row);
  const DTOFFEMMAP_ST& operator[](const size_t& row) const;
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
  void set_ch(size_t d0, size_t n, short v) {
    fTableData[n].ch[d0] = v;
  }
  short get_ch(size_t d0, size_t n) const {
    return fTableData[n].ch[d0];
  }

private:
  DTOFFEMMAP_ST* fTableData;

  ClassDef(dTofFEMmapWrapper,1)
};
#endif /*__DTOFFEMMAPWRAPPER_H__*/
