#ifndef __FKINWRAPPER_H__
#define __FKINWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "fkin.h"
class fkinWrapper: public PHTable
{
public:
  fkinWrapper(const char* name = "fkin", const size_t& max_rows = 1);
//  fkinWrapper(const fkinWrapper& source);
//  fkinWrapper& operator=(const fkinWrapper& source);

  ~fkinWrapper();

  void* RawTableData();
  FKIN_ST* TableData();

  FKIN_ST& operator[](const size_t& row);
  const FKIN_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_true_track(size_t n, int v) {
    fTableData[n].true_track = v;
  }
  int get_true_track(size_t n) const {
    return fTableData[n].true_track;
  }
  void set_subevent(size_t n, int v) {
    fTableData[n].subevent = v;
  }
  int get_subevent(size_t n) const {
    return fTableData[n].subevent;
  }
  void set_ntrack(size_t n, int v) {
    fTableData[n].ntrack = v;
  }
  int get_ntrack(size_t n) const {
    return fTableData[n].ntrack;
  }
  void set_ptot(size_t n, float v) {
    fTableData[n].ptot = v;
  }
  float get_ptot(size_t n) const {
    return fTableData[n].ptot;
  }
  void set_pthet(size_t n, float v) {
    fTableData[n].pthet = v;
  }
  float get_pthet(size_t n) const {
    return fTableData[n].pthet;
  }
  void set_pphi(size_t n, float v) {
    fTableData[n].pphi = v;
  }
  float get_pphi(size_t n) const {
    return fTableData[n].pphi;
  }
  void set_r_vertex(size_t n, float v) {
    fTableData[n].r_vertex = v;
  }
  float get_r_vertex(size_t n) const {
    return fTableData[n].r_vertex;
  }
  void set_z_vertex(size_t n, float v) {
    fTableData[n].z_vertex = v;
  }
  float get_z_vertex(size_t n) const {
    return fTableData[n].z_vertex;
  }
  void set_th_vertx(size_t n, float v) {
    fTableData[n].th_vertx = v;
  }
  float get_th_vertx(size_t n) const {
    return fTableData[n].th_vertx;
  }
  void set_ph_vertx(size_t n, float v) {
    fTableData[n].ph_vertx = v;
  }
  float get_ph_vertx(size_t n) const {
    return fTableData[n].ph_vertx;
  }
  void set_itparent(size_t n, int v) {
    fTableData[n].itparent = v;
  }
  int get_itparent(size_t n) const {
    return fTableData[n].itparent;
  }
  void set_idparent(size_t n, int v) {
    fTableData[n].idparent = v;
  }
  int get_idparent(size_t n) const {
    return fTableData[n].idparent;
  }
  void set_idpart(size_t n, int v) {
    fTableData[n].idpart = v;
  }
  int get_idpart(size_t n) const {
    return fTableData[n].idpart;
  }
  void set_nfile(size_t n, int v) {
    fTableData[n].nfile = v;
  }
  int get_nfile(size_t n) const {
    return fTableData[n].nfile;
  }

private:
  FKIN_ST* fTableData; //!

  ClassDef(fkinWrapper,1)
};
#endif /*__FKINWRAPPER_H__*/
