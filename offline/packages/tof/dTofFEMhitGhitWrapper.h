#ifndef __DTOFFEMHITGHITWRAPPER_H__
#define __DTOFFEMHITGHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofFEMhitGhit.h"
class dTofFEMhitGhitWrapper: public PHTable
{
public:
  dTofFEMhitGhitWrapper(const char* name = "dTofFEMhitGhit", const size_t& max_rows = 1);
  dTofFEMhitGhitWrapper(const dTofFEMhitGhitWrapper& source);
  dTofFEMhitGhitWrapper& operator=(const dTofFEMhitGhitWrapper& source);

  ~dTofFEMhitGhitWrapper();

  void* RawTableData();
  DTOFFEMHITGHIT_ST* TableData();

  DTOFFEMHITGHIT_ST& operator[](const size_t& row);
  const DTOFFEMHITGHIT_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_crate(size_t n, unsigned short v) {
    fTableData[n].crate = v;
  }
  unsigned short get_crate(size_t n) const {
    return fTableData[n].crate;
  }
  void set_slot(size_t n, unsigned short v) {
    fTableData[n].slot = v;
  }
  unsigned short get_slot(size_t n) const {
    return fTableData[n].slot;
  }
  void set_ch(size_t d0, size_t n, unsigned short v) {
    fTableData[n].ch[d0] = v;
  }
  unsigned short get_ch(size_t d0, size_t n) const {
    return fTableData[n].ch[d0];
  }
  void set_slatid(size_t n, short v) {
    fTableData[n].slatid = v;
  }
  short get_slatid(size_t n) const {
    return fTableData[n].slatid;
  }
  void set_ghitid(size_t n, short v) {
    fTableData[n].ghitid = v;
  }
  short get_ghitid(size_t n) const {
    return fTableData[n].ghitid;
  }

private:
  DTOFFEMHITGHIT_ST* fTableData;

  ClassDef(dTofFEMhitGhitWrapper,1)
};
#endif /*__DTOFFEMHITGHITWRAPPER_H__*/
