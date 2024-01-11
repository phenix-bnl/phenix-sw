#ifndef __DEMCRESPPARWRAPPER_H__
#define __DEMCRESPPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcRespPar.h"
class dEmcRespParWrapper: public PHTable
{
public:
  dEmcRespParWrapper(const char* name = "dEmcRespPar", const size_t& max_rows = 1);
//  dEmcRespParWrapper(const dEmcRespParWrapper& source);
//  dEmcRespParWrapper& operator=(const dEmcRespParWrapper& source);

  ~dEmcRespParWrapper();

  void* RawTableData();
  DEMCRESPPAR_ST* TableData();

  DEMCRESPPAR_ST& operator[](const size_t& row);
  const DEMCRESPPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_anyset(size_t n, int v) {
    fTableData[n].anyset = v;
  }
  int get_anyset(size_t n) const {
    return fTableData[n].anyset;
  }
  void set_sim_timing(size_t n, int v) {
    fTableData[n].sim_timing = v;
  }
  int get_sim_timing(size_t n) const {
    return fTableData[n].sim_timing;
  }
  void set_pbgl_response(size_t n, int v) {
    fTableData[n].pbgl_response = v;
  }
  int get_pbgl_response(size_t n) const {
    return fTableData[n].pbgl_response;
  }

private:
  DEMCRESPPAR_ST* fTableData;

  ClassDef(dEmcRespParWrapper,1)
};
#endif /*__DEMCRESPPARWRAPPER_H__*/
