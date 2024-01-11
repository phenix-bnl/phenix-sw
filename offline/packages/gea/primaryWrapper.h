#ifndef __PRIMARYWRAPPER_H__
#define __PRIMARYWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "primary.h"
class primaryWrapper: public PHTable
{
public:
  primaryWrapper(const char* name = "primary", const size_t& max_rows = 1);
//  primaryWrapper(const primaryWrapper& source);
//  primaryWrapper& operator=(const primaryWrapper& source);

  ~primaryWrapper();

  void* RawTableData();
  PRIMARY_ST* TableData();

  PRIMARY_ST& operator[](const size_t& row);
  const PRIMARY_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_key(size_t n, int v) {
    fTableData[n].key = v;
  }
  int get_key(size_t n) const {
    return fTableData[n].key;
  }
  void set_event_track(size_t n, int v) {
    fTableData[n].event_track = v;
  }
  int get_event_track(size_t n) const {
    return fTableData[n].event_track;
  }
  void set_subevent_track(size_t n, int v) {
    fTableData[n].subevent_track = v;
  }
  int get_subevent_track(size_t n) const {
    return fTableData[n].subevent_track;
  }
  void set_true_track(size_t n, int v) {
    fTableData[n].true_track = v;
  }
  int get_true_track(size_t n) const {
    return fTableData[n].true_track;
  }
  void set_subevent(size_t n, short v) {
    fTableData[n].subevent = v;
  }
  short get_subevent(size_t n) const {
    return fTableData[n].subevent;
  }
  void set_idpart(size_t n, short v) {
    fTableData[n].idpart = v;
  }
  short get_idpart(size_t n) const {
    return fTableData[n].idpart;
  }
  void set_nfile(size_t n, short v) {
    fTableData[n].nfile = v;
  }
  short get_nfile(size_t n) const {
    return fTableData[n].nfile;
  }
  void set_px_momentum(size_t n, float v) {
    fTableData[n].px_momentum = v;
  }
  float get_px_momentum(size_t n) const {
    return fTableData[n].px_momentum;
  }
  void set_py_momentum(size_t n, float v) {
    fTableData[n].py_momentum = v;
  }
  float get_py_momentum(size_t n) const {
    return fTableData[n].py_momentum;
  }
  void set_pz_momentum(size_t n, float v) {
    fTableData[n].pz_momentum = v;
  }
  float get_pz_momentum(size_t n) const {
    return fTableData[n].pz_momentum;
  }

private:
  PRIMARY_ST* fTableData; //! Suppress dictionary for this member (we supply the streamer)

  ClassDef(primaryWrapper,1)
};
#endif /*__PRIMARYWRAPPER_H__*/
