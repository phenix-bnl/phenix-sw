#ifndef __DCRKDCMPARWRAPPER_H__
#define __DCRKDCMPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkDCMpar.h"
class dCrkDCMparWrapper: public PHTable
{
public:
  dCrkDCMparWrapper(const char* name = "dCrkDCMpar", const size_t& max_rows = 1);
//  dCrkDCMparWrapper(const dCrkDCMparWrapper& source);
//  dCrkDCMparWrapper& operator=(const dCrkDCMparWrapper& source);

  ~dCrkDCMparWrapper();

  void* RawTableData();
  DCRKDCMPAR_ST* TableData();

  DCRKDCMPAR_ST& operator[](const size_t& row);
  const DCRKDCMPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_mode(size_t n, short v) {
    fTableData[n].mode = v;
  }
  short get_mode(size_t n) const {
    return fTableData[n].mode;
  }
  void set_threshold(size_t n, short v) {
    fTableData[n].threshold = v;
  }
  short get_threshold(size_t n) const {
    return fTableData[n].threshold;
  }

private:
  DCRKDCMPAR_ST* fTableData;

  ClassDef(dCrkDCMparWrapper,1)
};
#endif /*__DCRKDCMPARWRAPPER_H__*/
