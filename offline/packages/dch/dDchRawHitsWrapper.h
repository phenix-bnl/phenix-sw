#ifndef __DDCHRAWHITSWRAPPER_H__
#define __DDCHRAWHITSWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchRawHits.h"
class dDchRawHitsWrapper: public PHTable
{
public:
  dDchRawHitsWrapper(const char* name = "dDchRawHits", const size_t& max_rows = 1);
  ~dDchRawHitsWrapper();

  void* RawTableData();
  DDCHRAWHITS_ST* TableData();

  DDCHRAWHITS_ST& operator[](const size_t& row);
  const DDCHRAWHITS_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_rawid(size_t n, short v) {
    fTableData[n].rawid = v;
  }
  short get_rawid(size_t n) const {
    return fTableData[n].rawid;
  }
  void set_hitsid(size_t n, short v) {
    fTableData[n].hitsid = v;
  }
  short get_hitsid(size_t n) const {
    return fTableData[n].hitsid;
  }

private:
  DDCHRAWHITS_ST* fTableData;

  ClassDef(dDchRawHitsWrapper,1)
};
#endif /*__DDCHRAWHITSWRAPPER_H__*/
