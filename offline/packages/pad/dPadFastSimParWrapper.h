#ifndef __DPADFASTSIMPARWRAPPER_H__
#define __DPADFASTSIMPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadFastSimPar.h"
class dPadFastSimParWrapper: public PHTable
{
public:
  dPadFastSimParWrapper(const char* name = "dPadFastSimPar", const size_t& max_rows = 1);
//  dPadFastSimParWrapper(const dPadFastSimParWrapper& source);
//  dPadFastSimParWrapper& operator=(const dPadFastSimParWrapper& source);

  ~dPadFastSimParWrapper();

  void* RawTableData();
  DPADFASTSIMPAR_ST* TableData();

  DPADFASTSIMPAR_ST& operator[](const size_t& row);
  const DPADFASTSIMPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

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
  void set_efficiency(size_t d0, size_t n, float v) {
    fTableData[n].efficiency[d0] = v;
  }
  float get_efficiency(size_t d0, size_t n) const {
    return fTableData[n].efficiency[d0];
  }
  void set_phires(size_t d0, size_t n, float v) {
    fTableData[n].phires[d0] = v;
  }
  float get_phires(size_t d0, size_t n) const {
    return fTableData[n].phires[d0];
  }
  void set_zres(size_t d0, size_t n, float v) {
    fTableData[n].zres[d0] = v;
  }
  float get_zres(size_t d0, size_t n) const {
    return fTableData[n].zres[d0];
  }
  void set_phisep(size_t d0, size_t n, float v) {
    fTableData[n].phisep[d0] = v;
  }
  float get_phisep(size_t d0, size_t n) const {
    return fTableData[n].phisep[d0];
  }
  void set_zsep(size_t d0, size_t n, float v) {
    fTableData[n].zsep[d0] = v;
  }
  float get_zsep(size_t d0, size_t n) const {
    return fTableData[n].zsep[d0];
  }
  void set_verbose(size_t n, short v) {
    fTableData[n].verbose = v;
  }
  short get_verbose(size_t n) const {
    return fTableData[n].verbose;
  }

private:
  DPADFASTSIMPAR_ST* fTableData;

  ClassDef(dPadFastSimParWrapper,1)
};
#endif /*__DPADFASTSIMPARWRAPPER_H__*/
