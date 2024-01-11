#ifndef __DBBCDCMWRAPPER_H__
#define __DBBCDCMWRAPPER_H__
/* Automatically generated.  Do not edit. */
#include <stddef.h>
#include "PHTable.hh"
#include "dBbcDCM.h"
class dBbcDCMWrapper: public PHTable
{
public:
  dBbcDCMWrapper(const char* name = "dBbcDCM", const size_t& max_rows = 1);
  dBbcDCMWrapper(const dBbcDCMWrapper& source);
  dBbcDCMWrapper& operator=(const dBbcDCMWrapper& source);

  ~dBbcDCMWrapper();

  void* RawTableData();
  DBBCDCM_ST* TableData();

  DBBCDCM_ST& operator[](const size_t& row);
  const DBBCDCM_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_nWord(size_t n, unsigned long v) {
    fTableData[n].nWord = v;
  }
  unsigned long get_nWord(size_t n) const {
    return fTableData[n].nWord;
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
  DBBCDCM_ST* fTableData;

  ClassDef(dBbcDCMWrapper,1)
};
#endif /*__DBBCDCMWRAPPER_H__*/
