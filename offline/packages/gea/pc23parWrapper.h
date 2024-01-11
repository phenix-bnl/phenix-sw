#ifndef __PC23PARWRAPPER_H__
#define __PC23PARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "pc23par.h"
class pc23parWrapper: public PHTable
{
public:
  pc23parWrapper(const char* name = "pc23par", const size_t& max_rows = 1);
//  pc23parWrapper(const pc23parWrapper& source);
//  pc23parWrapper& operator=(const pc23parWrapper& source);

  ~pc23parWrapper();

  void* RawTableData();
  PC23PAR_ST* TableData();

  PC23PAR_ST& operator[](const size_t& row);
  const PC23PAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_idatePC23(size_t n, int v) {
    fTableData[n].idatePC23 = v;
  }
  int get_idatePC23(size_t n) const {
    return fTableData[n].idatePC23;
  }

private:
  PC23PAR_ST* fTableData;

  ClassDef(pc23parWrapper,1)
};
#endif /*__PC23PARWRAPPER_H__*/
