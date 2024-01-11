#ifndef __MUNHITSWRAPPER_H__
#define __MUNHITSWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "munhits.h"
class munhitsWrapper: public PHTable
{
public:
  munhitsWrapper(const char* name = "munhits", const size_t& max_rows = 1);
//  munhitsWrapper(const munhitsWrapper& source);
//  munhitsWrapper& operator=(const munhitsWrapper& source);

  ~munhitsWrapper();

  void* RawTableData();
  MUNHITS_ST* TableData();

  MUNHITS_ST& operator[](const size_t& row);
  const MUNHITS_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_tof(size_t n, float v) {
    fTableData[n].tof = v;
  }
  float get_tof(size_t n) const {
    return fTableData[n].tof;
  }
  void set_de(size_t n, float v) {
    fTableData[n].de = v;
  }
  float get_de(size_t n) const {
    return fTableData[n].de;
  }
  void set_rhit(size_t d0, size_t n, float v) {
    fTableData[n].rhit[d0] = v;
  }
  float get_rhit(size_t d0, size_t n) const {
    return fTableData[n].rhit[d0];
  }
  void set_phit(size_t d0, size_t n, float v) {
    fTableData[n].phit[d0] = v;
  }
  float get_phit(size_t d0, size_t n) const {
    return fTableData[n].phit[d0];
  }
  void set_track_num(size_t n, int v) {
    fTableData[n].track_num = v;
  }
  int get_track_num(size_t n) const {
    return fTableData[n].track_num;
  }
  void set_trk_id(size_t n, short v) {
    fTableData[n].trk_id = v;
  }
  short get_trk_id(size_t n) const {
    return fTableData[n].trk_id;
  }
  void set_plane_num(size_t n, short v) {
    fTableData[n].plane_num = v;
  }
  short get_plane_num(size_t n) const {
    return fTableData[n].plane_num;
  }
  void set_itrsub(size_t n, short v) {
    fTableData[n].itrsub = v;
  }
  short get_itrsub(size_t n) const {
    return fTableData[n].itrsub;
  }
  void set_itrksub(size_t n, short v) {
    fTableData[n].itrksub = v;
  }
  short get_itrksub(size_t n) const {
    return fTableData[n].itrksub;
  }

private:
  MUNHITS_ST* fTableData;

  ClassDef(munhitsWrapper,1)
};
#endif /*__MUNHITSWRAPPER_H__*/
