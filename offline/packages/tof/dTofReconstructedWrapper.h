#ifndef __DTOFRECONSTRUCTEDWRAPPER_H__
#define __DTOFRECONSTRUCTEDWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofReconstructed.h"
class dTofReconstructedWrapper: public PHTable
{
public:
  dTofReconstructedWrapper(const char* name = "dTofReconstructed", const size_t& max_rows = 1);
  dTofReconstructedWrapper(const dTofReconstructedWrapper& source);
  dTofReconstructedWrapper& operator=(const dTofReconstructedWrapper& source);

  ~dTofReconstructedWrapper();

  void* RawTableData();
  DTOFRECONSTRUCTED_ST* TableData();

  DTOFRECONSTRUCTED_ST& operator[](const size_t& row);
  const DTOFRECONSTRUCTED_ST& operator[](const size_t& row) const;
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
  void set_tof(size_t n, float v) {
    fTableData[n].tof = v;
  }
  float get_tof(size_t n) const {
    return fTableData[n].tof;
  }
  void set_tof_err(size_t n, float v) {
    fTableData[n].tof_err = v;
  }
  float get_tof_err(size_t n) const {
    return fTableData[n].tof_err;
  }
  void set_eloss(size_t n, float v) {
    fTableData[n].eloss = v;
  }
  float get_eloss(size_t n) const {
    return fTableData[n].eloss;
  }
  void set_eloss_err(size_t n, float v) {
    fTableData[n].eloss_err = v;
  }
  float get_eloss_err(size_t n) const {
    return fTableData[n].eloss_err;
  }
  void set_xtof(size_t d0, size_t n, float v) {
    fTableData[n].xtof[d0] = v;
  }
  float get_xtof(size_t d0, size_t n) const {
    return fTableData[n].xtof[d0];
  }
  void set_xtof_err(size_t d0, size_t n, float v) {
    fTableData[n].xtof_err[d0] = v;
  }
  float get_xtof_err(size_t d0, size_t n) const {
    return fTableData[n].xtof_err[d0];
  }
  void set_qvc(size_t d0, size_t n, short v) {
    fTableData[n].qvc[d0] = v;
  }
  short get_qvc(size_t d0, size_t n) const {
    return fTableData[n].qvc[d0];
  }
  void set_tvc(size_t d0, size_t n, short v) {
    fTableData[n].tvc[d0] = v;
  }
  short get_tvc(size_t d0, size_t n) const {
    return fTableData[n].tvc[d0];
  }
  float get_tdiff(size_t n) const {
    return fTableData[n].tdiff;
  }
  void set_tdiff(size_t n, float v) {
    fTableData[n].tdiff = v;
  }

private:
  DTOFRECONSTRUCTED_ST* fTableData;

  ClassDef(dTofReconstructedWrapper,1)
};
#endif /*__DTOFRECONSTRUCTEDWRAPPER_H__*/
