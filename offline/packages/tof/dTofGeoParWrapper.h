#ifndef __DTOFGEOPARWRAPPER_H__
#define __DTOFGEOPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofGeoPar.h"
class dTofGeoParWrapper: public PHTable
{
public:
  dTofGeoParWrapper(const char* name = "dTofGeoPar", const size_t& max_rows = 1);
  dTofGeoParWrapper(const dTofGeoParWrapper& source);
  dTofGeoParWrapper& operator=(const dTofGeoParWrapper& source);

  ~dTofGeoParWrapper();

  void* RawTableData();
  DTOFGEOPAR_ST* TableData();

  DTOFGEOPAR_ST& operator[](const size_t& row);
  const DTOFGEOPAR_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_rslat(size_t n, float v) {
    fTableData[n].rslat = v;
  }
  float get_rslat(size_t n) const {
    return fTableData[n].rslat;
  }
  void set_slat_width(size_t n, float v) {
    fTableData[n].slat_width = v;
  }
  float get_slat_width(size_t n) const {
    return fTableData[n].slat_width;
  }
  void set_scintz(size_t d0, size_t n, float v) {
    fTableData[n].scintz[d0] = v;
  }
  float get_scintz(size_t d0, size_t n) const {
    return fTableData[n].scintz[d0];
  }
  void set_rpos(size_t d0, size_t n, float v) {
    fTableData[n].rpos[d0] = v;
  }
  float get_rpos(size_t d0, size_t n) const {
    return fTableData[n].rpos[d0];
  }
  void set_phi(size_t d0, size_t n, float v) {
    fTableData[n].phi[d0] = v;
  }
  float get_phi(size_t d0, size_t n) const {
    return fTableData[n].phi[d0];
  }
  void set_zpos(size_t d0, size_t n, float v) {
    fTableData[n].zpos[d0] = v;
  }
  float get_zpos(size_t d0, size_t n) const {
    return fTableData[n].zpos[d0];
  }

private:
  DTOFGEOPAR_ST* fTableData;

  ClassDef(dTofGeoParWrapper,1)
};
#endif /*__DTOFGEOPARWRAPPER_H__*/
