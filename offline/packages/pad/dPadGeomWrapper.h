#ifndef __DPADGEOMWRAPPER_H__
#define __DPADGEOMWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadGeom.h"
class dPadGeomWrapper: public PHTable
{
public:
  dPadGeomWrapper(const char* name = "dPadGeom", const size_t& max_rows = 1);
//  dPadGeomWrapper(const dPadGeomWrapper& source);
//  dPadGeomWrapper& operator=(const dPadGeomWrapper& source);

  ~dPadGeomWrapper();

  void* RawTableData();
  DPADGEOM_ST* TableData();

  DPADGEOM_ST& operator[](const size_t& row);
  const DPADGEOM_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_pdxoff(size_t d0, size_t n, float v) {
    fTableData[n].pdxoff[d0] = v;
  }
  float get_pdxoff(size_t d0, size_t n) const {
    return fTableData[n].pdxoff[d0];
  }
  void set_pdzoff(size_t d0, size_t n, float v) {
    fTableData[n].pdzoff[d0] = v;
  }
  float get_pdzoff(size_t d0, size_t n) const {
    return fTableData[n].pdzoff[d0];
  }
  void set_pdgas(size_t d0, size_t n, float v) {
    fTableData[n].pdgas[d0] = v;
  }
  float get_pdgas(size_t d0, size_t n) const {
    return fTableData[n].pdgas[d0];
  }
  void set_aasep(size_t d0, size_t n, float v) {
    fTableData[n].aasep[d0] = v;
  }
  float get_aasep(size_t d0, size_t n) const {
    return fTableData[n].aasep[d0];
  }
  void set_pxlen(size_t d0, size_t n, float v) {
    fTableData[n].pxlen[d0] = v;
  }
  float get_pxlen(size_t d0, size_t n) const {
    return fTableData[n].pxlen[d0];
  }
  void set_wside(size_t d0, size_t n, float v) {
    fTableData[n].wside[d0] = v;
  }
  float get_wside(size_t d0, size_t n) const {
    return fTableData[n].wside[d0];
  }
  void set_wcent(size_t d0, size_t n, float v) {
    fTableData[n].wcent[d0] = v;
  }
  float get_wcent(size_t d0, size_t n) const {
    return fTableData[n].wcent[d0];
  }
  void set_pxsep(size_t d0, size_t n, float v) {
    fTableData[n].pxsep[d0] = v;
  }
  float get_pxsep(size_t d0, size_t n) const {
    return fTableData[n].pxsep[d0];
  }
  void set_clsep(size_t d0, size_t n, float v) {
    fTableData[n].clsep[d0] = v;
  }
  float get_clsep(size_t d0, size_t n) const {
    return fTableData[n].clsep[d0];
  }
  void set_npdsec(size_t d0, size_t n, short v) {
    fTableData[n].npdsec[d0] = v;
  }
  short get_npdsec(size_t d0, size_t n) const {
    return fTableData[n].npdsec[d0];
  }
  void set_npdwr(size_t d0, size_t n, short v) {
    fTableData[n].npdwr[d0] = v;
  }
  short get_npdwr(size_t d0, size_t n) const {
    return fTableData[n].npdwr[d0];
  }
  void set_npdx(size_t d0, size_t n, short v) {
    fTableData[n].npdx[d0] = v;
  }
  short get_npdx(size_t d0, size_t n) const {
    return fTableData[n].npdx[d0];
  }
  void set_npdz(size_t d0, size_t n, short v) {
    fTableData[n].npdz[d0] = v;
  }
  short get_npdz(size_t d0, size_t n) const {
    return fTableData[n].npdz[d0];
  }
  void set_sectperarm(size_t d0, size_t n, short v) {
    fTableData[n].sectperarm[d0] = v;
  }
  short get_sectperarm(size_t d0, size_t n) const {
    return fTableData[n].sectperarm[d0];
  }
  void set_inradius(size_t d0, size_t n, float v) {
    fTableData[n].inradius[d0] = v;
  }
  float get_inradius(size_t d0, size_t n) const {
    return fTableData[n].inradius[d0];
  }
  void set_zgap(size_t d0, size_t n, float v) {
    fTableData[n].zgap[d0] = v;
  }
  float get_zgap(size_t d0, size_t n) const {
    return fTableData[n].zgap[d0];
  }
  void set_phibote(size_t n, float v) {
    fTableData[n].phibote = v;
  }
  float get_phibote(size_t n) const {
    return fTableData[n].phibote;
  }
  void set_phitope(size_t n, float v) {
    fTableData[n].phitope = v;
  }
  float get_phitope(size_t n) const {
    return fTableData[n].phitope;
  }
  void set_phibotw(size_t n, float v) {
    fTableData[n].phibotw = v;
  }
  float get_phibotw(size_t n) const {
    return fTableData[n].phibotw;
  }
  void set_phitopw(size_t n, float v) {
    fTableData[n].phitopw = v;
  }
  float get_phitopw(size_t n) const {
    return fTableData[n].phitopw;
  }

private:
  DPADGEOM_ST* fTableData;

  ClassDef(dPadGeomWrapper,1)
};
#endif /*__DPADGEOMWRAPPER_H__*/
