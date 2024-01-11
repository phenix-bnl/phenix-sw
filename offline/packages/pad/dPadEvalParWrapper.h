#ifndef __DPADEVALPARWRAPPER_H__
#define __DPADEVALPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadEvalPar.h"
class dPadEvalParWrapper: public PHTable
{
public:
  dPadEvalParWrapper(const char* name = "dPadEvalPar", const size_t& max_rows = 1);
//  dPadEvalParWrapper(const dPadEvalParWrapper& source);
//  dPadEvalParWrapper& operator=(const dPadEvalParWrapper& source);

  ~dPadEvalParWrapper();

  void* RawTableData();
  DPADEVALPAR_ST* TableData();

  DPADEVALPAR_ST& operator[](const size_t& row);
  const DPADEVALPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_verbose(size_t n, short v) {
    fTableData[n].verbose = v;
  }
  short get_verbose(size_t n) const {
    return fTableData[n].verbose;
  }
  void set_fillclus(size_t n, short v) {
    fTableData[n].fillclus = v;
  }
  short get_fillclus(size_t n) const {
    return fTableData[n].fillclus;
  }
  void set_fillghit(size_t n, short v) {
    fTableData[n].fillghit = v;
  }
  short get_fillghit(size_t n) const {
    return fTableData[n].fillghit;
  }
  void set_filleval(size_t n, short v) {
    fTableData[n].filleval = v;
  }
  short get_filleval(size_t n) const {
    return fTableData[n].filleval;
  }
  void set_fillstat(size_t n, short v) {
    fTableData[n].fillstat = v;
  }
  short get_fillstat(size_t n) const {
    return fTableData[n].fillstat;
  }
  void set_printstat(size_t n, short v) {
    fTableData[n].printstat = v;
  }
  short get_printstat(size_t n) const {
    return fTableData[n].printstat;
  }
  void set_pcnumber(size_t n, short v) {
    fTableData[n].pcnumber = v;
  }
  short get_pcnumber(size_t n) const {
    return fTableData[n].pcnumber;
  }
  void set_rcutpc1(size_t n, float v) {
    fTableData[n].rcutpc1 = v;
  }
  float get_rcutpc1(size_t n) const {
    return fTableData[n].rcutpc1;
  }
  void set_rcutpc2(size_t n, float v) {
    fTableData[n].rcutpc2 = v;
  }
  float get_rcutpc2(size_t n) const {
    return fTableData[n].rcutpc2;
  }
  void set_rcutpc3(size_t n, float v) {
    fTableData[n].rcutpc3 = v;
  }
  float get_rcutpc3(size_t n) const {
    return fTableData[n].rcutpc3;
  }

private:
  DPADEVALPAR_ST* fTableData;

  ClassDef(dPadEvalParWrapper,1)
};
#endif /*__DPADEVALPARWRAPPER_H__*/
