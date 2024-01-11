#ifndef __HEADERWRAPPER_H__
#define __HEADERWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "header.h"
class headerWrapper: public PHTable
{
public:
  headerWrapper(const char* name = "header", const size_t& max_rows = 1);
//  headerWrapper(const headerWrapper& source);
//  headerWrapper& operator=(const headerWrapper& source);

  ~headerWrapper();

  void* RawTableData();
  HEADER_ST* TableData();

  HEADER_ST& operator[](const size_t& row);
  const HEADER_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_run(size_t n, short v) {
    fTableData[n].run = v;
  }
  short get_run(size_t n) const {
    return fTableData[n].run;
  }
  void set_event(size_t n, short v) {
    fTableData[n].event = v;
  }
  short get_event(size_t n) const {
    return fTableData[n].event;
  }
  void set_multiplicity(size_t n, short v) {
    fTableData[n].multiplicity = v;
  }
  short get_multiplicity(size_t n) const {
    return fTableData[n].multiplicity;
  }
  void set_b(size_t n, float v) {
    fTableData[n].b = v;
  }
  float get_b(size_t n) const {
    return fTableData[n].b;
  }
  void set_a1(size_t n, short v) {
    fTableData[n].a1 = v;
  }
  short get_a1(size_t n) const {
    return fTableData[n].a1;
  }
  void set_z1(size_t n, short v) {
    fTableData[n].z1 = v;
  }
  short get_z1(size_t n) const {
    return fTableData[n].z1;
  }
  void set_a2(size_t n, short v) {
    fTableData[n].a2 = v;
  }
  short get_a2(size_t n) const {
    return fTableData[n].a2;
  }
  void set_z2(size_t n, short v) {
    fTableData[n].z2 = v;
  }
  short get_z2(size_t n) const {
    return fTableData[n].z2;
  }
  void set_sqrt_s(size_t n, float v) {
    fTableData[n].sqrt_s = v;
  }
  float get_sqrt_s(size_t n) const {
    return fTableData[n].sqrt_s;
  }
  void set_bmin(size_t n, float v) {
    fTableData[n].bmin = v;
  }
  float get_bmin(size_t n) const {
    return fTableData[n].bmin;
  }
  void set_bmax(size_t n, float v) {
    fTableData[n].bmax = v;
  }
  float get_bmax(size_t n) const {
    return fTableData[n].bmax;
  }
  void set_t0femto(size_t n, float v) {
    fTableData[n].t0femto = v;
  }
  float get_t0femto(size_t n) const {
    return fTableData[n].t0femto;
  }
  void set_vertex(size_t d0, size_t n, float v) {
    fTableData[n].vertex[d0] = v;
  }
  float get_vertex(size_t d0, size_t n) const {
    return fTableData[n].vertex[d0];
  }
  void set_itime(size_t n, short v) {
    fTableData[n].itime = v;
  }
  short get_itime(size_t n) const {
    return fTableData[n].itime;
  }
  void set_idate(size_t n, int v) {
    fTableData[n].idate = v;
  }
  int get_idate(size_t n) const {
    return fTableData[n].idate;
  }
  void set_nrndm(size_t d0, size_t n, int v) {
    fTableData[n].nrndm[d0] = v;
  }
  int get_nrndm(size_t d0, size_t n) const {
    return fTableData[n].nrndm[d0];
  }
  void set_isqStart(size_t n, short v) {
    fTableData[n].isqStart = v;
  }
  short get_isqStart(size_t n) const {
    return fTableData[n].isqStart;
  }
  void set_iSeconds(size_t n, int v) {
    fTableData[n].iSeconds = v;
  }
  int get_iSeconds(size_t n) const {
    return fTableData[n].iSeconds;
  }
  void set_maxTrueTrack(size_t n, int v) {
    fTableData[n].maxTrueTrack = v;
  }
  int get_maxTrueTrack(size_t n) const {
    return fTableData[n].maxTrueTrack;
  }

private:
  HEADER_ST* fTableData; //! suppress the dict (we supply the streamer)

  ClassDef(headerWrapper,1)
};
#endif /*__HEADERWRAPPER_H__*/
