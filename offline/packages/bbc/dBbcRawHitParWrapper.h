#ifndef __DBBCRAWHITPARWRAPPER_H__
#define __DBBCRAWHITPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dBbcRawHitPar.h"
class dBbcRawHitParWrapper: public PHTable
{
public:
  dBbcRawHitParWrapper(const char* name = "dBbcRawHitPar", const size_t& max_rows = 1);
  dBbcRawHitParWrapper(const dBbcRawHitParWrapper& source);
  dBbcRawHitParWrapper& operator=(const dBbcRawHitParWrapper& source);

  ~dBbcRawHitParWrapper();

  void* RawTableData();
  DBBCRAWHITPAR_ST* TableData();

  DBBCRAWHITPAR_ST& operator[](const size_t& row);
  const DBBCRAWHITPAR_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_MinAdc(size_t n, short v) {
    fTableData[n].MinAdc = v;
  }
  short get_MinAdc(size_t n) const {
    return fTableData[n].MinAdc;
  }
  void set_MaxAdc(size_t n, short v) {
    fTableData[n].MaxAdc = v;
  }
  short get_MaxAdc(size_t n) const {
    return fTableData[n].MaxAdc;
  }
  void set_MinTdc0(size_t n, short v) {
    fTableData[n].MinTdc0 = v;
  }
  short get_MinTdc0(size_t n) const {
    return fTableData[n].MinTdc0;
  }
  void set_MaxTdc0(size_t n, short v) {
    fTableData[n].MaxTdc0 = v;
  }
  short get_MaxTdc0(size_t n) const {
    return fTableData[n].MaxTdc0;
  }
  void set_MinTdc1(size_t n, short v) {
    fTableData[n].MinTdc1 = v;
  }
  short get_MinTdc1(size_t n) const {
    return fTableData[n].MinTdc1;
  }
  void set_MaxTdc1(size_t n, short v) {
    fTableData[n].MaxTdc1 = v;
  }
  short get_MaxTdc1(size_t n) const {
    return fTableData[n].MaxTdc1;
  }

private:
  DBBCRAWHITPAR_ST* fTableData;

  ClassDef(dBbcRawHitParWrapper,1)
};
#endif /*__DBBCRAWHITPARWRAPPER_H__*/
