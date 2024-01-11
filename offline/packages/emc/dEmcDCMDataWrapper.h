#ifndef __DEMCDCMDATAWRAPPER_H__
#define __DEMCDCMDATAWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcDCMData.h"
class dEmcDCMDataWrapper: public PHTable
{
public:
  dEmcDCMDataWrapper(const char* name = "dEmcDCMData", const size_t& max_rows = 1);
//  dEmcDCMDataWrapper(const dEmcDCMDataWrapper& source);
//  dEmcDCMDataWrapper& operator=(const dEmcDCMDataWrapper& source);

  ~dEmcDCMDataWrapper();

  void* RawTableData();
  DEMCDCMDATA_ST* TableData();

  DEMCDCMDATA_ST& operator[](const size_t& row);
  const DEMCDCMDATA_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_nWords(size_t n, unsigned int v) {
    fTableData[n].nWords = v;
  }
  unsigned int get_nWords(size_t n) const {
    return fTableData[n].nWords;
  }
  void set_scheme(size_t n, unsigned int v) {
    fTableData[n].scheme = v;
  }
  unsigned int get_scheme(size_t n) const {
    return fTableData[n].scheme;
  }
  void set_packetID(size_t n, unsigned int v) {
    fTableData[n].packetID = v;
  }
  unsigned int get_packetID(size_t n) const {
    return fTableData[n].packetID;
  }
  void set_DCM(size_t d0, size_t n, unsigned int v) {
    fTableData[n].DCM[d0] = v;
  }
  unsigned int get_DCM(size_t d0, size_t n) const {
    return fTableData[n].DCM[d0];
  }

private:
  DEMCDCMDATA_ST* fTableData;

  ClassDef(dEmcDCMDataWrapper,1)
};
#endif /*__DEMCDCMDATAWRAPPER_H__*/
