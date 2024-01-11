#ifndef __DPADSLOWSIMPARWRAPPER_H__
#define __DPADSLOWSIMPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadSlowSimPar.h"
class dPadSlowSimParWrapper: public PHTable
{
public:
  dPadSlowSimParWrapper(const char* name = "dPadSlowSimPar", const size_t& max_rows = 1);
//  dPadSlowSimParWrapper(const dPadSlowSimParWrapper& source);
//  dPadSlowSimParWrapper& operator=(const dPadSlowSimParWrapper& source);

  ~dPadSlowSimParWrapper();

  void* RawTableData();
  DPADSLOWSIMPAR_ST* TableData();

  DPADSLOWSIMPAR_ST& operator[](const size_t& row);
  const DPADSLOWSIMPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_verbose(size_t n, short v) {
    fTableData[n].verbose = v;
  }
  short get_verbose(size_t n) const {
    return fTableData[n].verbose;
  }
  void set_pcnumber(size_t n, short v) {
    fTableData[n].pcnumber = v;
  }
  short get_pcnumber(size_t n) const {
    return fTableData[n].pcnumber;
  }
  void set_randseed(size_t d0, size_t n, int v) {
    fTableData[n].randseed[d0] = v;
  }
  int get_randseed(size_t d0, size_t n) const {
    return fTableData[n].randseed[d0];
  }
  void set_qnoise(size_t d0, size_t n, float v) {
    fTableData[n].qnoise[d0] = v;
  }
  float get_qnoise(size_t d0, size_t n) const {
    return fTableData[n].qnoise[d0];
  }
  void set_threshold(size_t d0, size_t n, float v) {
    fTableData[n].threshold[d0] = v;
  }
  float get_threshold(size_t d0, size_t n) const {
    return fTableData[n].threshold[d0];
  }

private:
  DPADSLOWSIMPAR_ST* fTableData;

  ClassDef(dPadSlowSimParWrapper,1)
};
#endif /*__DPADSLOWSIMPARWRAPPER_H__*/
