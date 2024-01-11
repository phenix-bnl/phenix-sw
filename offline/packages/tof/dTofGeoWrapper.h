#ifndef __DTOFGEOWRAPPER_H__
#define __DTOFGEOWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofGeo.h"
class dTofGeoWrapper: public PHTable
{
public:
  dTofGeoWrapper(const char* name = "dTofGeo", const size_t& max_rows = 1);
  dTofGeoWrapper(const dTofGeoWrapper& source);
  dTofGeoWrapper& operator=(const dTofGeoWrapper& source);

  ~dTofGeoWrapper();

  void* RawTableData();
  DTOFGEO_ST* TableData();

  DTOFGEO_ST& operator[](const size_t& row);
  const DTOFGEO_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_slatid(size_t n, short v) {
    fTableData[n].slatid = v;
  }
  short get_slatid(size_t n) const {
    return fTableData[n].slatid;
  }
  void set_sector(size_t n, short v) {
    fTableData[n].sector = v;
  }
  short get_sector(size_t n) const {
    return fTableData[n].sector;
  }
  void set_side(size_t n, short v) {
    fTableData[n].side = v;
  }
  short get_side(size_t n) const {
    return fTableData[n].side;
  }
  void set_panel(size_t n, short v) {
    fTableData[n].panel = v;
  }
  short get_panel(size_t n) const {
    return fTableData[n].panel;
  }
  void set_slat(size_t n, short v) {
    fTableData[n].slat = v;
  }
  short get_slat(size_t n) const {
    return fTableData[n].slat;
  }
  void set_pos(size_t d0, size_t n, float v) {
    fTableData[n].pos[d0] = v;
  }
  float get_pos(size_t d0, size_t n) const {
    return fTableData[n].pos[d0];
  }
  void set_r(size_t n, float v) {
    fTableData[n].r = v;
  }
  float get_r(size_t n) const {
    return fTableData[n].r;
  }
  void set_phi(size_t n, float v) {
    fTableData[n].phi = v;
  }
  float get_phi(size_t n) const {
    return fTableData[n].phi;
  }

private:
  DTOFGEO_ST* fTableData;

  ClassDef(dTofGeoWrapper,1)
};
#endif /*__DTOFGEOWRAPPER_H__*/
