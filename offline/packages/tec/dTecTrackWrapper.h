#ifndef __DTECTRACKWRAPPER_H__
#define __DTECTRACKWRAPPER_H__

#include <cstddef>
#include "PHTable.hh"
#include "dTecTrack.h"

#include "PHPoint.h"
#include "PHPanel.h"

class dTecTrackWrapper: public PHTable
{
public:
  dTecTrackWrapper(const char* name = "dTecTrack", const size_t& max_rows = 1);
//  dTecTrackWrapper(const dTecTrackWrapper& source);
//  dTecTrackWrapper& operator=(const dTecTrackWrapper& source);

  ~dTecTrackWrapper();

  void* RawTableData();
  DTECTRACK_ST* TableData();

  DTECTRACK_ST& operator[](const size_t& row);
  const DTECTRACK_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_xyzin(size_t d0, size_t n, float v) {
    fTableData[n].xyzin[d0] = v;
  }
  float get_xyzin(size_t d0, size_t n) const {
    return fTableData[n].xyzin[d0];
  }
  void set_xyzout(size_t d0, size_t n, float v) {
    fTableData[n].xyzout[d0] = v;
  }
  float get_xyzout(size_t d0, size_t n) const {
    return fTableData[n].xyzout[d0];
  }
  void set_dxyin(size_t d0, size_t n, float v) {
    fTableData[n].dxyin[d0] = v;
  }
  float get_dxyin(size_t d0, size_t n) const {
    return fTableData[n].dxyin[d0];
  }
  void set_dxyout(size_t d0, size_t n, float v) {
    fTableData[n].dxyout[d0] = v;
  }
  float get_dxyout(size_t d0, size_t n) const {
    return fTableData[n].dxyout[d0];
  }
  void set_quality(size_t n, float v) {
    fTableData[n].quality = v;
  }
  float get_quality(size_t n) const {
    return fTableData[n].quality;
  }
  void set_nhits(size_t n, short v) {
    fTableData[n].nhits = v;
  }
  short get_nhits(size_t n) const {
    return fTableData[n].nhits;
  }
  void set_ntime(size_t n, short v) {
    fTableData[n].ntime = v;
  }
  short get_ntime(size_t n) const {
    return fTableData[n].ntime;
  }
  void set_pid(size_t n, short v) {
    fTableData[n].pid = v;
  }
  short get_pid(size_t n) const {
    return fTableData[n].pid;
  }
  void set_pidqual(size_t n, float v) {
    fTableData[n].pidqual = v;
  }
  float get_pidqual(size_t n) const {
    return fTableData[n].pidqual;
  }
  int get_NFwires(size_t n);
  int get_NFplanes(size_t n);

private:
  DTECTRACK_ST* fTableData;

  ClassDef(dTecTrackWrapper,1)
};
#endif /*__DTECTRACKWRAPPER_H__*/
