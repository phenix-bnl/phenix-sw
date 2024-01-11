#ifndef __DTOFASSOCPARWRAPPER_H__
#define __DTOFASSOCPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofAssocPar.h"
class dTofAssocParWrapper: public PHTable
{
public:
  dTofAssocParWrapper(const char* name = "dTofAssocPar", const size_t& max_rows = 1);
  dTofAssocParWrapper(const dTofAssocParWrapper& source);
  dTofAssocParWrapper& operator=(const dTofAssocParWrapper& source);

  ~dTofAssocParWrapper();

  void* RawTableData();
  DTOFASSOCPAR_ST* TableData();

  DTOFASSOCPAR_ST& operator[](const size_t& row);
  const DTOFASSOCPAR_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_factor(size_t n, float v) {
    fTableData[n].factor = v;
  }
  float get_factor(size_t n) const {
    return fTableData[n].factor;
  }
  void set_sigma_xy(size_t n, float v) {
    fTableData[n].sigma_xy = v;
  }
  float get_sigma_xy(size_t n) const {
    return fTableData[n].sigma_xy;
  }
  void set_sigma_z(size_t n, float v) {
    fTableData[n].sigma_z = v;
  }
  float get_sigma_z(size_t n) const {
    return fTableData[n].sigma_z;
  }

private:
  DTOFASSOCPAR_ST* fTableData;

  ClassDef(dTofAssocParWrapper,1)
};
#endif /*__DTOFASSOCPARWRAPPER_H__*/
