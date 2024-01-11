#ifndef __DEMCGEAHITWRAPPER_H__
#define __DEMCGEAHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcGeaHit.h"
class dEmcGeaHitWrapper: public PHTable
{
public:
  dEmcGeaHitWrapper(const char* name = "dEmcGeaHit", const size_t& max_rows = 1);
//  dEmcGeaHitWrapper(const dEmcGeaHitWrapper& source);
//  dEmcGeaHitWrapper& operator=(const dEmcGeaHitWrapper& source);

  ~dEmcGeaHitWrapper();

  void* RawTableData();
  DEMCGEAHIT_ST* TableData();

  DEMCGEAHIT_ST& operator[](const size_t& row);
  const DEMCGEAHIT_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, int v) {
    fTableData[n].id = v;
  }
  int get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_type(size_t n, short v) {
    fTableData[n].type = v;
  }
  short get_type(size_t n) const {
    return fTableData[n].type;
  }
  void set_sector(size_t n, short v) {
    fTableData[n].sector = v;
  }
  short get_sector(size_t n) const {
    return fTableData[n].sector;
  }
  void set_smodind(size_t n, short v) {
    fTableData[n].smodind = v;
  }
  short get_smodind(size_t n) const {
    return fTableData[n].smodind;
  }
  void set_towerind(size_t n, short v) {
    fTableData[n].towerind = v;
  }
  short get_towerind(size_t n) const {
    return fTableData[n].towerind;
  }
  void set_deltae(size_t n, float v) {
    fTableData[n].deltae = v;
  }
  float get_deltae(size_t n) const {
    return fTableData[n].deltae;
  }
  void set_xyz(size_t d0, size_t n, float v) {
    fTableData[n].xyz[d0] = v;
  }
  float get_xyz(size_t d0, size_t n) const {
    return fTableData[n].xyz[d0];
  }
  void set_tof(size_t n, float v) {
    fTableData[n].tof = v;
  }
  float get_tof(size_t n) const {
    return fTableData[n].tof;
  }
  void set_numed(size_t n, short v) {
    fTableData[n].numed = v;
  }
  short get_numed(size_t n) const {
    return fTableData[n].numed;
  }
  void set_partid(size_t n, short v) {
    fTableData[n].partid = v;
  }
  short get_partid(size_t n) const {
    return fTableData[n].partid;
  }
  void set_itrack(size_t n, short v) {
    fTableData[n].itrack = v;
  }
  short get_itrack(size_t n) const {
    return fTableData[n].itrack;
  }
  void set_isubevt(size_t n, short v) {
    fTableData[n].isubevt = v;
  }
  short get_isubevt(size_t n) const {
    return fTableData[n].isubevt;
  }
  void set_nfile(size_t n, short v) {
    fTableData[n].nfile = v;
  }
  short get_nfile(size_t n) const {
    return fTableData[n].nfile;
  }
  void set_true_track(size_t n, int v) {
    fTableData[n].true_track = v;
  }
  int get_true_track(size_t n) const {
    return fTableData[n].true_track;
  }

private:
  DEMCGEAHIT_ST* fTableData;

  ClassDef(dEmcGeaHitWrapper,1)
};
#endif /*__DEMCGEAHITWRAPPER_H__*/
