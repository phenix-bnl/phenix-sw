#ifndef __DDCHTRACKSEXTWRAPPER_H__
#define __DDCHTRACKSEXTWRAPPER_H__
/* Automatically generated.  Do not edit. */
#include <stddef.h>
#include "PHTable.hh"
#include "dDchTracksExt.h"
class dDchTracksExtWrapper: public PHTable
{
public:
  dDchTracksExtWrapper(const char* name = "dDchTracksExt", const size_t& max_rows = 1);
  dDchTracksExtWrapper(const dDchTracksExtWrapper& source);
  dDchTracksExtWrapper& operator=(const dDchTracksExtWrapper& source);

  ~dDchTracksExtWrapper();

  void* RawTableData();
  DDCHTRACKSEXT_ST* TableData();

  DDCHTRACKSEXT_ST& operator[](const size_t& row);
  const DDCHTRACKSEXT_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_status(size_t n, short v) {
    fTableData[n].status = v;
  }
  short get_status(size_t n) const {
    return fTableData[n].status;
  }
  void set_alpha1(size_t n, float v) {
    fTableData[n].alpha1 = v;
  }
  float get_alpha1(size_t n) const {
    return fTableData[n].alpha1;
  }
  void set_alpha2(size_t n, float v) {
    fTableData[n].alpha2 = v;
  }
  float get_alpha2(size_t n) const {
    return fTableData[n].alpha2;
  }
  void set_dist1(size_t n, float v) {
    fTableData[n].dist1 = v;
  }
  float get_dist1(size_t n) const {
    return fTableData[n].dist1;
  }
  void set_dist2(size_t n, float v) {
    fTableData[n].dist2 = v;
  }
  float get_dist2(size_t n) const {
    return fTableData[n].dist2;
  }
  void set_chi21(size_t n, float v) {
    fTableData[n].chi21 = v;
  }
  float get_chi21(size_t n) const {
    return fTableData[n].chi21;
  }
  void set_chi22(size_t n, float v) {
    fTableData[n].chi22 = v;
  }
  float get_chi22(size_t n) const {
    return fTableData[n].chi22;
  }
  void set_nx1hits(size_t n, char v) {
    fTableData[n].nx1hits = v;
  }
  char get_nx1hits(size_t n) const {
    return fTableData[n].nx1hits;
  }
  void set_nx2hits(size_t n, char v) {
    fTableData[n].nx2hits = v;
  }
  char get_nx2hits(size_t n) const {
    return fTableData[n].nx2hits;
  }

private:
  DDCHTRACKSEXT_ST* fTableData;

  ClassDef(dDchTracksExtWrapper,1)
};
#endif /*__DDCHTRACKSEXTWRAPPER_H__*/
