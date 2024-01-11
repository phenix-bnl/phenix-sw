#ifndef __DDCHFASTSIMPARWRAPPER_H__
#define __DDCHFASTSIMPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchFastSimPar.h"
class dDchFastSimParWrapper: public PHTable
{
public:
  dDchFastSimParWrapper(const char* name = "dDchFastSimPar", const size_t& max_rows = 1);
  ~dDchFastSimParWrapper();

  void* RawTableData();
  DDCHFASTSIMPAR_ST* TableData();

  DDCHFASTSIMPAR_ST& operator[](const size_t& row);
  const DDCHFASTSIMPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_randseed(size_t n, int v) {
    fTableData[n].randseed = v;
  }
  int get_randseed(size_t n) const {
    return fTableData[n].randseed;
  }
  void set_wire_eff(size_t n, float v) {
    fTableData[n].wire_eff = v;
  }
  float get_wire_eff(size_t n) const {
    return fTableData[n].wire_eff;
  }
  void set_rphires(size_t n, float v) {
    fTableData[n].rphires = v;
  }
  float get_rphires(size_t n) const {
    return fTableData[n].rphires;
  }
  void set_rphiprop(size_t n, float v) {
    fTableData[n].rphiprop = v;
  }
  float get_rphiprop(size_t n) const {
    return fTableData[n].rphiprop;
  }
  void set_twotrksep(size_t n, float v) {
    fTableData[n].twotrksep = v;
  }
  float get_twotrksep(size_t n) const {
    return fTableData[n].twotrksep;
  }
  void set_back_eff(size_t n, float v) {
    fTableData[n].back_eff = v;
  }
  float get_back_eff(size_t n) const {
    return fTableData[n].back_eff;
  }
  void set_verbose(size_t n, short v) {
    fTableData[n].verbose = v;
  }
  short get_verbose(size_t n) const {
    return fTableData[n].verbose;
  }
  void set_testchain(size_t n, short v) {
    fTableData[n].testchain = v;
  }
  short get_testchain(size_t n) const {
    return fTableData[n].testchain;
  }

private:
  DDCHFASTSIMPAR_ST* fTableData;

  ClassDef(dDchFastSimParWrapper,1)
};
#endif /*__DDCHFASTSIMPARWRAPPER_H__*/
