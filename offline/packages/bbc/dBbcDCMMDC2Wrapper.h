#ifndef __DBBCDCMMDC2WRAPPER_H__
#define __DBBCDCMMDC2WRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dBbcDCMMDC2.h"
class dBbcDCMMDC2Wrapper: public PHTable
{
public:
  dBbcDCMMDC2Wrapper(const char* name = "dBbcDCMMDC2", const size_t& max_rows = 1);
  dBbcDCMMDC2Wrapper(const dBbcDCMMDC2Wrapper& source);
  dBbcDCMMDC2Wrapper& operator=(const dBbcDCMMDC2Wrapper& source);

  ~dBbcDCMMDC2Wrapper();

  void* RawTableData();
  DBBCDCMMDC2_ST* TableData();

  DBBCDCMMDC2_ST& operator[](const size_t& row);
  const DBBCDCMMDC2_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_nWord(size_t n, unsigned int v) {
    fTableData[n].nWord = v;
  }
  unsigned int get_nWord(size_t n) const {
    return fTableData[n].nWord;
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
  DBBCDCMMDC2_ST* fTableData;

  ClassDef(dBbcDCMMDC2Wrapper,1)
};
#endif /*__DBBCDCMMDC2WRAPPER_H__*/
