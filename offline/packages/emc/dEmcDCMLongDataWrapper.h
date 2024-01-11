#ifndef __DEMCDCMLONGDATAWRAPPER_H__
#define __DEMCDCMLONGDATAWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcDCMLongData.h"
class dEmcDCMLongDataWrapper: public PHTable
{
public:
  dEmcDCMLongDataWrapper(const char* name = "dEmcDCMLongData", const size_t& max_rows = 1);
  dEmcDCMLongDataWrapper(const dEmcDCMLongDataWrapper& source);
  dEmcDCMLongDataWrapper& operator=(const dEmcDCMLongDataWrapper& source);

  ~dEmcDCMLongDataWrapper();

  void* RawTableData();
  DEMCDCMLONGDATA_ST* TableData();

  DEMCDCMLONGDATA_ST& operator[](const size_t& row);
  const DEMCDCMLONGDATA_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_nWords(size_t n, unsigned long v) {
    fTableData[n].nWords = v;
  }
  unsigned long get_nWords(size_t n) const {
    return fTableData[n].nWords;
  }
  void set_scheme(size_t n, unsigned long v) {
    fTableData[n].scheme = v;
  }
  unsigned long get_scheme(size_t n) const {
    return fTableData[n].scheme;
  }
  void set_packetID(size_t n, unsigned long v) {
    fTableData[n].packetID = v;
  }
  unsigned long get_packetID(size_t n) const {
    return fTableData[n].packetID;
  }
  void set_DCM(size_t d0, size_t n, unsigned long v) {
    fTableData[n].DCM[d0] = v;
  }
  unsigned long get_DCM(size_t d0, size_t n) const {
    return fTableData[n].DCM[d0];
  }

private:
  DEMCDCMLONGDATA_ST* fTableData;

  ClassDef(dEmcDCMLongDataWrapper,1)
};
#endif /*__DEMCDCMLONGDATAWRAPPER_H__*/
