#ifndef __DTOFRAWRECWRAPPER_H__
#define __DTOFRAWRECWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofRawRec.h"
class dTofRawRecWrapper: public PHTable
{
public:
  dTofRawRecWrapper(const char* name = "dTofRawRec", const size_t& max_rows = 1);
  dTofRawRecWrapper(const dTofRawRecWrapper& source);
  dTofRawRecWrapper& operator=(const dTofRawRecWrapper& source);

  ~dTofRawRecWrapper();

  void* RawTableData();
  DTOFRAWREC_ST* TableData();

  DTOFRAWREC_ST& operator[](const size_t& row);
  const DTOFRAWREC_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_rawid(size_t n, short v) {
    fTableData[n].rawid = v;
  }
  short get_rawid(size_t n) const {
    return fTableData[n].rawid;
  }
  void set_slatid(size_t n, short v) {
    fTableData[n].slatid = v;
  }
  short get_slatid(size_t n) const {
    return fTableData[n].slatid;
  }
  void set_recid(size_t n, short v) {
    fTableData[n].recid = v;
  }
  short get_recid(size_t n) const {
    return fTableData[n].recid;
  }

private:
  DTOFRAWREC_ST* fTableData;

  ClassDef(dTofRawRecWrapper,1)
};
#endif /*__DTOFRAWRECWRAPPER_H__*/
