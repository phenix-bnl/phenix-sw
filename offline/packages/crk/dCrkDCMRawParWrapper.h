#ifndef __DCRKDCMRAWPARWRAPPER_H__
#define __DCRKDCMRAWPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkDCMRawPar.h"
class dCrkDCMRawParWrapper: public PHTable
{
public:
  dCrkDCMRawParWrapper(const char* name = "dCrkDCMRawPar", const size_t& max_rows = 1);
//  dCrkDCMRawParWrapper(const dCrkDCMRawParWrapper& source);
//  dCrkDCMRawParWrapper& operator=(const dCrkDCMRawParWrapper& source);

  ~dCrkDCMRawParWrapper();

  void* RawTableData();
  DCRKDCMRAWPAR_ST* TableData();

  DCRKDCMRAWPAR_ST& operator[](const size_t& row);
  const DCRKDCMRAWPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_threshold(size_t n, short v) {
    fTableData[n].threshold = v;
  }
  short get_threshold(size_t n) const {
    return fTableData[n].threshold;
  }

private:
  DCRKDCMRAWPAR_ST* fTableData;

  ClassDef(dCrkDCMRawParWrapper,1)
};
#endif /*__DCRKDCMRAWPARWRAPPER_H__*/
