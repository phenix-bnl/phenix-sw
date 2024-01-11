#ifndef __DPAD23PARWRAPPER_H__
#define __DPAD23PARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPad23Par.h"
class dPad23ParWrapper: public PHTable
{
public:
  dPad23ParWrapper(const char* name = "dPad23Par", const size_t& max_rows = 1);
//  dPad23ParWrapper(const dPad23ParWrapper& source);
//  dPad23ParWrapper& operator=(const dPad23ParWrapper& source);

  ~dPad23ParWrapper();

  void* RawTableData();
  DPAD23PAR_ST* TableData();

  DPAD23PAR_ST& operator[](const size_t& row);
  const DPAD23PAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_idatePC23(size_t n, int v) {
    fTableData[n].idatePC23 = v;
  }
  int get_idatePC23(size_t n) const {
    return fTableData[n].idatePC23;
  }

private:
  DPAD23PAR_ST* fTableData;

  ClassDef(dPad23ParWrapper,1)
};
#endif /*__DPAD23PARWRAPPER_H__*/
