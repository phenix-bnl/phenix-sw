#ifndef __DPADGHITCLUSWRAPPER_H__
#define __DPADGHITCLUSWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadGhitClus.h"
class dPadGhitClusWrapper: public PHTable
{
public:
  dPadGhitClusWrapper(const char* name = "dPadGhitClus", const size_t& max_rows = 1);
//  dPadGhitClusWrapper(const dPadGhitClusWrapper& source);
//  dPadGhitClusWrapper& operator=(const dPadGhitClusWrapper& source);

  ~dPadGhitClusWrapper();

  void* RawTableData();
  DPADGHITCLUS_ST* TableData();

  DPADGHITCLUS_ST& operator[](const size_t& row);
  const DPADGHITCLUS_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_ghitid(size_t n, short v) {
    fTableData[n].ghitid = v;
  }
  short get_ghitid(size_t n) const {
    return fTableData[n].ghitid;
  }
  void set_clusid(size_t n, short v) {
    fTableData[n].clusid = v;
  }
  short get_clusid(size_t n) const {
    return fTableData[n].clusid;
  }

private:
  DPADGHITCLUS_ST* fTableData;

  ClassDef(dPadGhitClusWrapper,1)
};
#endif /*__DPADGHITCLUSWRAPPER_H__*/
