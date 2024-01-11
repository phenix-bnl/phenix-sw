#ifndef __CRKPARUWRAPPER_H__
#define __CRKPARUWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "crkparu.h"
class crkparuWrapper: public PHTable
{
public:
  crkparuWrapper(const char* name = "crkparu", const size_t& max_rows = 1);
//  crkparuWrapper(const crkparuWrapper& source);
//  crkparuWrapper& operator=(const crkparuWrapper& source);

  ~crkparuWrapper();

  void* RawTableData();
  CRKPARU_ST* TableData();

  CRKPARU_ST& operator[](const size_t& row);
  const CRKPARU_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_pntr(size_t n, short v) {
    fTableData[n].pntr = v;
  }
  short get_pntr(size_t n) const {
    return fTableData[n].pntr;
  }
  void set_RAD_GAS(size_t n, float v) {
    fTableData[n].RAD_GAS = v;
  }
  float get_RAD_GAS(size_t n) const {
    return fTableData[n].RAD_GAS;
  }
  void set_N0init(size_t n, float v) {
    fTableData[n].N0init = v;
  }
  float get_N0init(size_t n) const {
    return fTableData[n].N0init;
  }

private:
  CRKPARU_ST* fTableData;

  ClassDef(crkparuWrapper,1)
};
#endif /*__CRKPARUWRAPPER_H__*/
