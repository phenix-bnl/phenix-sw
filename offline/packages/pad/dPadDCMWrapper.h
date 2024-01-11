#ifndef __DPADDCMWRAPPER_H__
#define __DPADDCMWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadDCM.h"
class dPadDCMWrapper: public PHTable
{
public:
  dPadDCMWrapper(const char* name = "dPadDCM", const size_t& max_rows = 1);
//  dPadDCMWrapper(const dPadDCMWrapper& source);
//  dPadDCMWrapper& operator=(const dPadDCMWrapper& source);

  ~dPadDCMWrapper();

  void* RawTableData();
  DPADDCM_ST* TableData();

  DPADDCM_ST& operator[](const size_t& row);
  const DPADDCM_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_Word(size_t d0, size_t n, unsigned int v) {
    fTableData[n].Word[d0] = v;
  }
  unsigned int get_Word(size_t d0, size_t n) const {
    return fTableData[n].Word[d0];
  }

private:
  DPADDCM_ST* fTableData;

  ClassDef(dPadDCMWrapper,1)
};
#endif /*__DPADDCMWRAPPER_H__*/
