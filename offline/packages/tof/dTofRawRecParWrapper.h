#ifndef __DTOFRAWRECPARWRAPPER_H__
#define __DTOFRAWRECPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofRawRecPar.h"
class dTofRawRecParWrapper: public PHTable
{
public:
  dTofRawRecParWrapper(const char* name = "dTofRawRecPar", const size_t& max_rows = 1);
  dTofRawRecParWrapper(const dTofRawRecParWrapper& source);
  dTofRawRecParWrapper& operator=(const dTofRawRecParWrapper& source);

  ~dTofRawRecParWrapper();

  void* RawTableData();
  DTOFRAWRECPAR_ST* TableData();

  DTOFRAWRECPAR_ST& operator[](const size_t& row);
  const DTOFRAWRECPAR_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_verbose(size_t n, short v) {
    fTableData[n].verbose = v;
  }
  short get_verbose(size_t n) const {
    return fTableData[n].verbose;
  }

private:
  DTOFRAWRECPAR_ST* fTableData;

  ClassDef(dTofRawRecParWrapper,1)
};
#endif /*__DTOFRAWRECPARWRAPPER_H__*/
