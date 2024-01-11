#ifndef __DCRKGHITRAWPARWRAPPER_H__
#define __DCRKGHITRAWPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkGhitRawPar.h"
class dCrkGhitRawParWrapper: public PHTable
{
public:
  dCrkGhitRawParWrapper(const char* name = "dCrkGhitRawPar", const size_t& max_rows = 1);
//  dCrkGhitRawParWrapper(const dCrkGhitRawParWrapper& source);
//  dCrkGhitRawParWrapper& operator=(const dCrkGhitRawParWrapper& source);

  ~dCrkGhitRawParWrapper();

  void* RawTableData();
  DCRKGHITRAWPAR_ST* TableData();

  DCRKGHITRAWPAR_ST& operator[](const size_t& row);
  const DCRKGHITRAWPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_ghitraw(size_t n, short v) {
    fTableData[n].ghitraw = v;
  }
  short get_ghitraw(size_t n) const {
    return fTableData[n].ghitraw;
  }
  void set_max_adc(size_t n, short v) {
    fTableData[n].max_adc = v;
  }
  short get_max_adc(size_t n) const {
    return fTableData[n].max_adc;
  }
  void set_max_tdc(size_t n, short v) {
    fTableData[n].max_tdc = v;
  }
  short get_max_tdc(size_t n) const {
    return fTableData[n].max_tdc;
  }
  void set_min_tdc(size_t n, short v) {
    fTableData[n].min_tdc = v;
  }
  short get_min_tdc(size_t n) const {
    return fTableData[n].min_tdc;
  }
  void set_N0_pisa(size_t n, float v) {
    fTableData[n].N0_pisa = v;
  }
  float get_N0_pisa(size_t n) const {
    return fTableData[n].N0_pisa;
  }
  void set_sinTmax(size_t n, float v) {
    fTableData[n].sinTmax = v;
  }
  float get_sinTmax(size_t n) const {
    return fTableData[n].sinTmax;
  }

private:
  DCRKGHITRAWPAR_ST* fTableData;

  ClassDef(dCrkGhitRawParWrapper,1)
};
#endif /*__DCRKGHITRAWPARWRAPPER_H__*/
