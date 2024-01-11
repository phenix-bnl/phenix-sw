#ifndef __DEMCGEATRACKWRAPPER_H__
#define __DEMCGEATRACKWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcGeaTrack.h"
class dEmcGeaTrackWrapper: public PHTable
{
public:
  dEmcGeaTrackWrapper(const char* name = "dEmcGeaTrack", const size_t& max_rows = 1);
//  dEmcGeaTrackWrapper(const dEmcGeaTrackWrapper& source);
//  dEmcGeaTrackWrapper& operator=(const dEmcGeaTrackWrapper& source);

  ~dEmcGeaTrackWrapper();

  void* RawTableData();
  DEMCGEATRACK_ST* TableData();

  DEMCGEATRACK_ST& operator[](const size_t& row);
  const DEMCGEATRACK_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, int v) {
    fTableData[n].id = v;
  }
  int get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_trkno(size_t n, int v) {
    fTableData[n].trkno = v;
  }
  int get_trkno(size_t n) const {
    return fTableData[n].trkno;
  }
  void set_input(size_t n, short v) {
    fTableData[n].input = v;
  }
  short get_input(size_t n) const {
    return fTableData[n].input;
  }
  void set_anclvl(size_t n, short v) {
    fTableData[n].anclvl = v;
  }
  short get_anclvl(size_t n) const {
    return fTableData[n].anclvl;
  }
  void set_pid(size_t n, short v) {
    fTableData[n].pid = v;
  }
  short get_pid(size_t n) const {
    return fTableData[n].pid;
  }
  void set_ekin(size_t n, float v) {
    fTableData[n].ekin = v;
  }
  float get_ekin(size_t n) const {
    return fTableData[n].ekin;
  }
  void set_xyz(size_t d0, size_t n, float v) {
    fTableData[n].xyz[d0] = v;
  }
  float get_xyz(size_t d0, size_t n) const {
    return fTableData[n].xyz[d0];
  }
  void set_ptot(size_t n, float v) {
    fTableData[n].ptot = v;
  }
  float get_ptot(size_t n) const {
    return fTableData[n].ptot;
  }
  void set_pxyz(size_t d0, size_t n, float v) {
    fTableData[n].pxyz[d0] = v;
  }
  float get_pxyz(size_t d0, size_t n) const {
    return fTableData[n].pxyz[d0];
  }
  void set_impxyz(size_t d0, size_t n, float v) {
    fTableData[n].impxyz[d0] = v;
  }
  float get_impxyz(size_t d0, size_t n) const {
    return fTableData[n].impxyz[d0];
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
  void set_parent_ptr(size_t n, int v) {
    fTableData[n].parent_ptr = v;
  }
  int get_parent_ptr(size_t n) const {
    return fTableData[n].parent_ptr;
  }
  void set_twrhit(size_t n, int v) {
    fTableData[n].twrhit = v;
  }
  int get_twrhit(size_t n) const {
    return fTableData[n].twrhit;
  }
  void set_edep(size_t n, float v) {
    fTableData[n].edep = v;
  }
  float get_edep(size_t n) const {
    return fTableData[n].edep;
  }

private:
  DEMCGEATRACK_ST* fTableData;

  ClassDef(dEmcGeaTrackWrapper,1)
};
#endif /*__DEMCGEATRACKWRAPPER_H__*/
