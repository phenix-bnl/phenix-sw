#ifndef __DEMCCALIBTOWERWRAPPER_H__
#define __DEMCCALIBTOWERWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcCalibTower.h"
class dEmcCalibTowerWrapper: public PHTable
{
public:
  dEmcCalibTowerWrapper(const char* name = "dEmcCalibTower", const size_t& max_rows = 1);
//  dEmcCalibTowerWrapper(const dEmcCalibTowerWrapper& source);
//  dEmcCalibTowerWrapper& operator=(const dEmcCalibTowerWrapper& source);

  ~dEmcCalibTowerWrapper();

  void* RawTableData();
  DEMCCALIBTOWER_ST* TableData();

  DEMCCALIBTOWER_ST& operator[](const size_t& row);
  const DEMCCALIBTOWER_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_hwkey(size_t n, int v) {
    fTableData[n].hwkey = v;
  }
  int get_hwkey(size_t n) const {
    return fTableData[n].hwkey;
  }
  void set_swkey(size_t n, int v) {
    fTableData[n].swkey = v;
  }
  int get_swkey(size_t n) const {
    return fTableData[n].swkey;
  }
  void set_type(size_t n, short v) {
    fTableData[n].type = v;
  }
  short get_type(size_t n) const {
    return fTableData[n].type;
  }
  void set_arm(size_t n, short v) {
    fTableData[n].arm = v;
  }
  short get_arm(size_t n) const {
    return fTableData[n].arm;
  }
  void set_sector(size_t n, short v) {
    fTableData[n].sector = v;
  }
  short get_sector(size_t n) const {
    return fTableData[n].sector;
  }
  void set_ind(size_t d0, size_t n, short v) {
    fTableData[n].ind[d0] = v;
  }
  short get_ind(size_t d0, size_t n) const {
    return fTableData[n].ind[d0];
  }
  void set_ecal(size_t n, float v) {
    fTableData[n].ecal = v;
  }
  float get_ecal(size_t n) const {
    return fTableData[n].ecal;
  }
  void set_tof(size_t n, float v) {
    fTableData[n].tof = v;
  }
  float get_tof(size_t n) const {
    return fTableData[n].tof;
  }
  void set_deadmap(size_t n, int v) {
    fTableData[n].deadmap = v;
  }
  int get_deadmap(size_t n) const {
    return fTableData[n].deadmap;
  }

  /* MV 2001/12/04 */
  void set_warnmap(size_t n, int v) {
    fTableData[n].warnmap = v;
  }
  int get_warnmap(size_t n) const {
    return fTableData[n].warnmap;
  }

  /* MV 2001/09/25 */
  void set_adc(size_t n, float adc){
    fTableData[n].adc = adc;
  }
  void set_tac(size_t n, float tac){
    fTableData[n].tac = tac;
  }
  float get_adc(size_t n){
    return fTableData[n].adc;
  }
  float get_tac(size_t n){
    return fTableData[n].tac;
  }

private:
  DEMCCALIBTOWER_ST* fTableData;

  // MV 2001/12/11 incremented class version 1 -> 2
  ClassDef(dEmcCalibTowerWrapper,2)
};
#endif /*__DEMCCALIBTOWERWRAPPER_H__*/
