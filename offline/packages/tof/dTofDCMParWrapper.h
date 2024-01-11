#ifndef __DTOFDCMPARWRAPPER_H__
#define __DTOFDCMPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofDCMPar.h"
class dTofDCMParWrapper: public PHTable
{
public:
  dTofDCMParWrapper(const char* name = "dTofDCMPar", const size_t& max_rows = 1);
  dTofDCMParWrapper(const dTofDCMParWrapper& source);
  dTofDCMParWrapper& operator=(const dTofDCMParWrapper& source);

  ~dTofDCMParWrapper();

  void* RawTableData();
  DTOFDCMPAR_ST* TableData();

  DTOFDCMPAR_ST& operator[](const size_t& row);
  const DTOFDCMPAR_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const; 

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_scheme(size_t n, unsigned int v) {
    fTableData[n].scheme = v;
  }
  unsigned int get_scheme(size_t n) const {
    return fTableData[n].scheme;
  }

private:
  DTOFDCMPAR_ST* fTableData;

  ClassDef(dTofDCMParWrapper,1)
};
#endif /*__DTOFDCMPARWRAPPER_H__*/
