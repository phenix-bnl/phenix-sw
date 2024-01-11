#ifndef __DCRKRAWHITPARWRAPPER_H__
#define __DCRKRAWHITPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkRawHitPar.h"
class dCrkRawHitParWrapper: public PHTable
{
public:
  dCrkRawHitParWrapper(const char* name = "dCrkRawHitPar", const size_t& max_rows = 1);
//  dCrkRawHitParWrapper(const dCrkRawHitParWrapper& source);
//  dCrkRawHitParWrapper& operator=(const dCrkRawHitParWrapper& source);

  ~dCrkRawHitParWrapper();

  void* RawTableData();
  DCRKRAWHITPAR_ST* TableData();

  DCRKRAWHITPAR_ST& operator[](const size_t& row);
  const DCRKRAWHITPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_min_pe(size_t n, float v) {
    fTableData[n].min_pe = v;
  }
  float get_min_pe(size_t n) const {
    return fTableData[n].min_pe;
  }

private:
  DCRKRAWHITPAR_ST* fTableData;

  ClassDef(dCrkRawHitParWrapper,1)
};
#endif /*__DCRKRAWHITPARWRAPPER_H__*/
