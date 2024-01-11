#ifndef __DTOFGHITRAWPARWRAPPER_H__
#define __DTOFGHITRAWPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofGhitRawPar.h"
class dTofGhitRawParWrapper: public PHTable
{
public:
  dTofGhitRawParWrapper(const char* name = "dTofGhitRawPar", const size_t& max_rows = 1);
  dTofGhitRawParWrapper(const dTofGhitRawParWrapper& source);
  dTofGhitRawParWrapper& operator=(const dTofGhitRawParWrapper& source);

  ~dTofGhitRawParWrapper();

  void* RawTableData();
  DTOFGHITRAWPAR_ST* TableData();

  DTOFGHITRAWPAR_ST& operator[](const size_t& row);
  const DTOFGHITRAWPAR_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_randseed(size_t n, int v) {
    fTableData[n].randseed = v;
  }
  int get_randseed(size_t n) const {
    return fTableData[n].randseed;
  }
  void set_verbose(size_t n, short v) {
    fTableData[n].verbose = v;
  }
  short get_verbose(size_t n) const {
    return fTableData[n].verbose;
  }
  void set_min_cell(size_t n, short v) {
    fTableData[n].min_cell = v;
  }
  short get_min_cell(size_t n) const {
    return fTableData[n].min_cell;
  }
  void set_max_cell(size_t n, short v) {
    fTableData[n].max_cell = v;
  }
  short get_max_cell(size_t n) const {
    return fTableData[n].max_cell;
  }
  void set_min_qvc(size_t n, short v) {
    fTableData[n].min_qvc = v;
  }
  short get_min_qvc(size_t n) const {
    return fTableData[n].min_qvc;
  }
  void set_max_qvc(size_t n, short v) {
    fTableData[n].max_qvc = v;
  }
  short get_max_qvc(size_t n) const {
    return fTableData[n].max_qvc;
  }
  void set_min_tvc(size_t n, short v) {
    fTableData[n].min_tvc = v;
  }
  short get_min_tvc(size_t n) const {
    return fTableData[n].min_tvc;
  }
  void set_max_tvc(size_t n, short v) {
    fTableData[n].max_tvc = v;
  }
  short get_max_tvc(size_t n) const {
    return fTableData[n].max_tvc;
  }

private:
  DTOFGHITRAWPAR_ST* fTableData;

  ClassDef(dTofGhitRawParWrapper,1)
};
#endif /*__DTOFGHITRAWPARWRAPPER_H__*/
