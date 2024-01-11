#ifndef __DEMCEVENTWRAPPER_H__
#define __DEMCEVENTWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcEvent.h"
class dEmcEventWrapper: public PHTable
{
public:
  dEmcEventWrapper(const char* name = "dEmcEvent", const size_t& max_rows = 1);
//  dEmcEventWrapper(const dEmcEventWrapper& source);
//  dEmcEventWrapper& operator=(const dEmcEventWrapper& source);

  ~dEmcEventWrapper();

  void* RawTableData();
  DEMCEVENT_ST* TableData();

  DEMCEVENT_ST& operator[](const size_t& row);
  const DEMCEVENT_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_evtyp(size_t n, short v) {
    fTableData[n].evtyp = v;
  }
  short get_evtyp(size_t n) const {
    return fTableData[n].evtyp;
  }
  void set_evno(size_t n, int v) {
    fTableData[n].evno = v;
  }
  int get_evno(size_t n) const {
    return fTableData[n].evno;
  }
  void set_runno(size_t n, int v) {
    fTableData[n].runno = v;
  }
  int get_runno(size_t n) const {
    return fTableData[n].runno;
  }
  void set_serialno(size_t n, short v) {
    fTableData[n].serialno = v;
  }
  short get_serialno(size_t n) const {
    return fTableData[n].serialno;
  }
  void set_impact(size_t n, float v) {
    fTableData[n].impact = v;
  }
  float get_impact(size_t n) const {
    return fTableData[n].impact;
  }
  void set_xyz(size_t d0, size_t n, float v) {
    fTableData[n].xyz[d0] = v;
  }
  float get_xyz(size_t d0, size_t n) const {
    return fTableData[n].xyz[d0];
  }
  void set_twrmultlo(size_t n, float v) {
    fTableData[n].twrmultlo = v;
  }
  float get_twrmultlo(size_t n) const {
    return fTableData[n].twrmultlo;
  }
  void set_twrmulthi(size_t n, float v) {
    fTableData[n].twrmulthi = v;
  }
  float get_twrmulthi(size_t n) const {
    return fTableData[n].twrmulthi;
  }
  void set_tote(size_t n, float v) {
    fTableData[n].tote = v;
  }
  float get_tote(size_t n) const {
    return fTableData[n].tote;
  }
  void set_totet(size_t n, float v) {
    fTableData[n].totet = v;
  }
  float get_totet(size_t n) const {
    return fTableData[n].totet;
  }
  void set_trigsum(size_t d0, size_t n, float v) {
    fTableData[n].trigsum[d0] = v;
  }
  float get_trigsum(size_t d0, size_t n) const {
    return fTableData[n].trigsum[d0];
  }
  void set_sece(size_t d0, size_t n, float v) {
    fTableData[n].sece[d0] = v;
  }
  float get_sece(size_t d0, size_t n) const {
    return fTableData[n].sece[d0];
  }
  void set_secet(size_t d0, size_t n, float v) {
    fTableData[n].secet[d0] = v;
  }
  float get_secet(size_t d0, size_t n) const {
    return fTableData[n].secet[d0];
  }

private:
  DEMCEVENT_ST* fTableData;

  ClassDef(dEmcEventWrapper,1)
};
#endif /*__DEMCEVENTWRAPPER_H__*/
