#ifndef __DDCHUNPACKPARWRAPPER_H__
#define __DDCHUNPACKPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchUnpackPar.h"
class dDchUnpackParWrapper: public PHTable
{
public:
  dDchUnpackParWrapper(const char* name = "dDchUnpackPar", const size_t& max_rows = 1);
  ~dDchUnpackParWrapper();

  void* RawTableData();
  DDCHUNPACKPAR_ST* TableData();

  DDCHUNPACKPAR_ST& operator[](const size_t& row);
  const DDCHUNPACKPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_detIdWord(size_t n, short v) {
    fTableData[n].detIdWord = v;
  }
  short get_detIdWord(size_t n) const {
    return fTableData[n].detIdWord;
  }
  void set_dc111(size_t n, short v) {
    fTableData[n].dc111 = v;
  }
  short get_dc111(size_t n) const {
    return fTableData[n].dc111;
  }

private:
  DDCHUNPACKPAR_ST* fTableData;

  ClassDef(dDchUnpackParWrapper,1)
};
#endif /*__DDCHUNPACKPARWRAPPER_H__*/
