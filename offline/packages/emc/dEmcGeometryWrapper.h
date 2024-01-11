#ifndef __DEMCGEOMETRYWRAPPER_H__
#define __DEMCGEOMETRYWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcGeometry.h"
class dEmcGeometryWrapper: public PHTable
{
public:
  dEmcGeometryWrapper(const char* name = "dEmcGeometry", const size_t& max_rows = 1);
//  dEmcGeometryWrapper(const dEmcGeometryWrapper& source);
//  dEmcGeometryWrapper& operator=(const dEmcGeometryWrapper& source);

  ~dEmcGeometryWrapper();

  void* RawTableData();
  DEMCGEOMETRY_ST* TableData();

  DEMCGEOMETRY_ST& operator[](const size_t& row);
  const DEMCGEOMETRY_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_hwkey(size_t n, short v) {
    fTableData[n].hwkey = v;
  }
  short get_hwkey(size_t n) const {
    return fTableData[n].hwkey;
  }
  void set_swkey(size_t n, short v) {
    fTableData[n].swkey = v;
  }
  short get_swkey(size_t n) const {
    return fTableData[n].swkey;
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
  void set_nomxyz(size_t d0, size_t n, float v) {
    fTableData[n].nomxyz[d0] = v;
  }
  float get_nomxyz(size_t d0, size_t n) const {
    return fTableData[n].nomxyz[d0];
  }
  void set_actxyz(size_t d0, size_t n, float v) {
    fTableData[n].actxyz[d0] = v;
  }
  float get_actxyz(size_t d0, size_t n) const {
    return fTableData[n].actxyz[d0];
  }
  void set_nomunitv(size_t d0, size_t n, float v) {
    fTableData[n].nomunitv[d0] = v;
  }
  float get_nomunitv(size_t d0, size_t n) const {
    return fTableData[n].nomunitv[d0];
  }
  void set_actunitv(size_t d0, size_t n, float v) {
    fTableData[n].actunitv[d0] = v;
  }
  float get_actunitv(size_t d0, size_t n) const {
    return fTableData[n].actunitv[d0];
  }
  void set_nomtheta(size_t n, float v) {
    fTableData[n].nomtheta = v;
  }
  float get_nomtheta(size_t n) const {
    return fTableData[n].nomtheta;
  }
  void set_nomphi(size_t n, float v) {
    fTableData[n].nomphi = v;
  }
  float get_nomphi(size_t n) const {
    return fTableData[n].nomphi;
  }
  void set_acttheta(size_t n, float v) {
    fTableData[n].acttheta = v;
  }
  float get_acttheta(size_t n) const {
    return fTableData[n].acttheta;
  }
  void set_actphi(size_t n, float v) {
    fTableData[n].actphi = v;
  }
  float get_actphi(size_t n) const {
    return fTableData[n].actphi;
  }
  void set_nomdist(size_t n, float v) {
    fTableData[n].nomdist = v;
  }
  float get_nomdist(size_t n) const {
    return fTableData[n].nomdist;
  }
  void set_actdist(size_t n, float v) {
    fTableData[n].actdist = v;
  }
  float get_actdist(size_t n) const {
    return fTableData[n].actdist;
  }
  void set_nomflash(size_t n, float v) {
    fTableData[n].nomflash = v;
  }
  float get_nomflash(size_t n) const {
    return fTableData[n].nomflash;
  }
  void set_actflash(size_t n, float v) {
    fTableData[n].actflash = v;
  }
  float get_actflash(size_t n) const {
    return fTableData[n].actflash;
  }
  void set_sectheta(size_t n, float v) {
    fTableData[n].sectheta = v;
  }
  float get_sectheta(size_t n) const {
    return fTableData[n].sectheta;
  }
  void set_secphi(size_t n, float v) {
    fTableData[n].secphi = v;
  }
  float get_secphi(size_t n) const {
    return fTableData[n].secphi;
  }

private:
  DEMCGEOMETRY_ST* fTableData;

  ClassDef(dEmcGeometryWrapper,1)
};
#endif /*__DEMCGEOMETRYWRAPPER_H__*/
