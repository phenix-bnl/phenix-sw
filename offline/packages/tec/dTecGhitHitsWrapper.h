#ifndef __DTECGHITHITSWRAPPER_H__
#define __DTECGHITHITSWRAPPER_H__

#include <cstddef>
#include "PHTable.hh"
#include "dTecGhitHits.h"
class dTecGhitHitsWrapper: public PHTable
{
public:
  dTecGhitHitsWrapper(const char* name = "dTecGhitHits", const size_t& max_rows = 1);
//  dTecGhitHitsWrapper(const dTecGhitHitsWrapper& source);
//  dTecGhitHitsWrapper& operator=(const dTecGhitHitsWrapper& source);

  ~dTecGhitHitsWrapper();

  void* RawTableData();
  DTECGHITHITS_ST* TableData();

  DTECGHITHITS_ST& operator[](const size_t& row);
  const DTECGHITHITS_ST& operator[](const size_t& row) const;

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
  DTECGHITHITS_ST* fTableData;

  ClassDef(dTecGhitHitsWrapper,1)
};
#endif /*__DTECGHITHITSWRAPPER_H__*/
