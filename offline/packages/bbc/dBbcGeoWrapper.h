#ifndef __DBBCGEOWRAPPER_H__
#define __DBBCGEOWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dBbcGeo.h"
class dBbcGeoWrapper: public PHTable
{
public:
  dBbcGeoWrapper(const char* name = "dBbcGeo", const size_t& max_rows = 1);
  dBbcGeoWrapper(const dBbcGeoWrapper& source);
  dBbcGeoWrapper& operator=(const dBbcGeoWrapper& source);

  ~dBbcGeoWrapper();

  void* RawTableData();
  DBBCGEO_ST* TableData();

  DBBCGEO_ST& operator[](const size_t& row);
  const DBBCGEO_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_MaxPmtNo(size_t n, short v) {
    fTableData[n].MaxPmtNo = v;
  }
  short get_MaxPmtNo(size_t n) const {
    return fTableData[n].MaxPmtNo;
  }
  void set_Ring(size_t d0, size_t n, short v) {
    fTableData[n].Ring[d0] = v;
  }
  short get_Ring(size_t d0, size_t n) const {
    return fTableData[n].Ring[d0];
  }
  void set_PMT(size_t d0, size_t n, short v) {
    fTableData[n].PMT[d0] = v;
  }
  short get_PMT(size_t d0, size_t n) const {
    return fTableData[n].PMT[d0];
  }

private:
  DBBCGEO_ST* fTableData;

  ClassDef(dBbcGeoWrapper,1)
};
#endif /*__DBBCGEOWRAPPER_H__*/
