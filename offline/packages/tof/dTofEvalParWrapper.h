#ifndef __DTOFEVALPARWRAPPER_H__
#define __DTOFEVALPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofEvalPar.h"
class dTofEvalParWrapper: public PHTable
{
public:
  dTofEvalParWrapper(const char* name = "dTofEvalPar", const size_t& max_rows = 1);
  dTofEvalParWrapper(const dTofEvalParWrapper& source);
  dTofEvalParWrapper& operator=(const dTofEvalParWrapper& source);

  ~dTofEvalParWrapper();

  void* RawTableData();
  DTOFEVALPAR_ST* TableData();

  DTOFEVALPAR_ST& operator[](const size_t& row);
  const DTOFEVALPAR_ST& operator[](const size_t& row) const;
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
  DTOFEVALPAR_ST* fTableData;

  ClassDef(dTofEvalParWrapper,1)
};
#endif /*__DTOFEVALPARWRAPPER_H__*/
