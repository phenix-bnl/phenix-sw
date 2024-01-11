#ifndef __DDCHGHITHITSWRAPPER_H__
#define __DDCHGHITHITSWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchGhitHits.h"
class dDchGhitHitsWrapper: public PHTable
{
public:
  dDchGhitHitsWrapper(const char* name = "dDchGhitHits", const size_t& max_rows = 1);
  ~dDchGhitHitsWrapper();

  void* RawTableData();
  DDCHGHITHITS_ST* TableData();

  DDCHGHITHITS_ST& operator[](const size_t& row);
  const DDCHGHITHITS_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_ghitid(size_t n, short v) {
    fTableData[n].ghitid = v;
  }
  short get_ghitid(size_t n) const {
    return fTableData[n].ghitid;
  }
  void set_hitsid(size_t n, short v) {
    fTableData[n].hitsid = v;
  }
  short get_hitsid(size_t n) const {
    return fTableData[n].hitsid;
  }

private:
  DDCHGHITHITS_ST* fTableData;

  ClassDef(dDchGhitHitsWrapper,1)
};
#endif /*__DDCHGHITHITSWRAPPER_H__*/
