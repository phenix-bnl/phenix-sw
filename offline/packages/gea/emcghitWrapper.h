#ifndef __EMCGHITWRAPPER_H__
#define __EMCGHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "emcghit.h"
class emcghitWrapper: public PHTable
{
public:
  emcghitWrapper(const char* name = "emcghit", const size_t& max_rows = 1);
//  emcghitWrapper(const emcghitWrapper& source);
//  emcghitWrapper& operator=(const emcghitWrapper& source);

  ~emcghitWrapper();

  void* RawTableData();
  EMCGHIT_ST* TableData();

  EMCGHIT_ST& operator[](const size_t& row);
  const EMCGHIT_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_iSubEvent(size_t n, short v) {
    fTableData[n].iSubEvent = v;
  }
  short get_iSubEvent(size_t n) const {
    return fTableData[n].iSubEvent;
  }
  void set_nFile(size_t n, short v) {
    fTableData[n].nFile = v;
  }
  short get_nFile(size_t n) const {
    return fTableData[n].nFile;
  }
  void set_iword(size_t d0, size_t n, int v) {
    fTableData[n].iword[d0] = v;
  }
  int get_iword(size_t d0, size_t n) const {
    return fTableData[n].iword[d0];
  }

private:
  EMCGHIT_ST* fTableData;

  ClassDef(emcghitWrapper,1)
};
#endif /*__EMCGHITWRAPPER_H__*/
