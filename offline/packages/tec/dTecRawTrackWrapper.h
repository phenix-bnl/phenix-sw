#ifndef __DTECRAWTRACKWRAPPER_H__
#define __DTECRAWTRACKWRAPPER_H__

#include <cstddef>
#include "PHTable.hh"
#include "dTecRawTrack.h"
class dTecRawTrackWrapper: public PHTable
{
public:
  dTecRawTrackWrapper(const char* name = "dTecRawTrack", const size_t& max_rows = 1);
//  dTecRawTrackWrapper(const dTecRawTrackWrapper& source);
//  dTecRawTrackWrapper& operator=(const dTecRawTrackWrapper& source);

  ~dTecRawTrackWrapper();

  void* RawTableData();
  DTECRAWTRACK_ST* TableData();

  DTECRAWTRACK_ST& operator[](const size_t& row);
  const DTECRAWTRACK_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_index(size_t n, short v) {
    fTableData[n].index = v;
  }
  short get_index(size_t n) const {
    return fTableData[n].index;
  }
  void set_plane(size_t n, short v) {
    fTableData[n].plane = v;
  }
  short get_plane(size_t n) const {
    return fTableData[n].plane;
  }
  void set_wire(size_t n, short v) {
    fTableData[n].wire = v;
  }
  short get_wire(size_t n) const {
    return fTableData[n].wire;
  }
  void set_bin(size_t n, short v) {
    fTableData[n].bin = v;
  }
  short get_bin(size_t n) const {
    return fTableData[n].bin;
  }
  void set_ampl(size_t n, float v) {
    fTableData[n].ampl = v;
  }
  float get_ampl(size_t n) const {
    return fTableData[n].ampl;
  }

private:
  DTECRAWTRACK_ST* fTableData;

  ClassDef(dTecRawTrackWrapper,1)
};
#endif /*__DTECRAWTRACKWRAPPER_H__*/
