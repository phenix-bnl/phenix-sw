#ifndef __DPADNIBBLEGHITWRAPPER_H__
#define __DPADNIBBLEGHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadNibbleGhit.h"
class dPadNibbleGhitWrapper: public PHTable
{
public:
  dPadNibbleGhitWrapper(const char* name = "dPadNibbleGhit", const size_t& max_rows = 1);
//  dPadNibbleGhitWrapper(const dPadNibbleGhitWrapper& source);
//  dPadNibbleGhitWrapper& operator=(const dPadNibbleGhitWrapper& source);

  ~dPadNibbleGhitWrapper();

  void* RawTableData();
  DPADNIBBLEGHIT_ST* TableData();

  DPADNIBBLEGHIT_ST& operator[](const size_t& row);
  const DPADNIBBLEGHIT_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_ghitid(size_t n, short v) {
    fTableData[n].ghitid = v;
  }
  short get_ghitid(size_t n) const {
    return fTableData[n].ghitid;
  }
  void set_rawid(size_t n, short v) {
    fTableData[n].rawid = v;
  }
  short get_rawid(size_t n) const {
    return fTableData[n].rawid;
  }
  void set_Card(size_t n, short v) {
    fTableData[n].Card = v;
  }
  short get_Card(size_t n) const {
    return fTableData[n].Card;
  }
  void set_padx(size_t n, short v) {
    fTableData[n].padx = v;
  }
  short get_padx(size_t n) const {
    return fTableData[n].padx;
  }
  void set_padz(size_t n, short v) {
    fTableData[n].padz = v;
  }
  short get_padz(size_t n) const {
    return fTableData[n].padz;
  }

private:
  DPADNIBBLEGHIT_ST* fTableData;

  ClassDef(dPadNibbleGhitWrapper,1)
};
#endif /*__DPADNIBBLEGHITWRAPPER_H__*/
