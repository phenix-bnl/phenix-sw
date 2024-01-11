#ifndef __DCRKREL2SWRAPPER_H__
#define __DCRKREL2SWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkRel2s.h"
class dCrkRel2sWrapper: public PHTable
{
public:
  dCrkRel2sWrapper(const char* name = "dCrkRel2s", const size_t& max_rows = 1);
//  dCrkRel2sWrapper(const dCrkRel2sWrapper& source);
//  dCrkRel2sWrapper& operator=(const dCrkRel2sWrapper& source);

  ~dCrkRel2sWrapper();

  void* RawTableData();
  DCRKREL2S_ST* TableData();

  DCRKREL2S_ST& operator[](const size_t& row);
  const DCRKREL2S_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id1(size_t n, short v) {
    fTableData[n].id1 = v;
  }
  short get_id1(size_t n) const {
    return fTableData[n].id1;
  }
  void set_id2(size_t n, short v) {
    fTableData[n].id2 = v;
  }
  short get_id2(size_t n) const {
    return fTableData[n].id2;
  }

private:
  DCRKREL2S_ST* fTableData;

  ClassDef(dCrkRel2sWrapper,1)
};
#endif /*__DCRKREL2SWRAPPER_H__*/
