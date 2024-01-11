#ifndef __DPADRAWCLUSWRAPPER_H__
#define __DPADRAWCLUSWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadRawClus.h"
class dPadRawClusWrapper: public PHTable
{
public:
  dPadRawClusWrapper(const char* name = "dPadRawClus", const size_t& max_rows = 1);
//  dPadRawClusWrapper(const dPadRawClusWrapper& source);
//  dPadRawClusWrapper& operator=(const dPadRawClusWrapper& source);

  ~dPadRawClusWrapper();

  void* RawTableData();
  DPADRAWCLUS_ST* TableData();

  DPADRAWCLUS_ST& operator[](const size_t& row);
  const DPADRAWCLUS_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_rawid(size_t n, short v) {
    fTableData[n].rawid = v;
  }
  short get_rawid(size_t n) const {
    return fTableData[n].rawid;
  }
  void set_clusid(size_t n, short v) {
    fTableData[n].clusid = v;
  }
  short get_clusid(size_t n) const {
    return fTableData[n].clusid;
  }

private:
  DPADRAWCLUS_ST* fTableData;

  ClassDef(dPadRawClusWrapper,1)
};
#endif /*__DPADRAWCLUSWRAPPER_H__*/
