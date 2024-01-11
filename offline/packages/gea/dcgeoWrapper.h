#ifndef __DCGEOWRAPPER_H__
#define __DCGEOWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dcgeo.h"
class dcgeoWrapper: public PHTable
{
public:
  dcgeoWrapper(const char* name = "dcgeo", const size_t& max_rows = 1);
//  dcgeoWrapper(const dcgeoWrapper& source);
//  dcgeoWrapper& operator=(const dcgeoWrapper& source);

  ~dcgeoWrapper();

  void* RawTableData();
  DCGEO_ST* TableData();

  DCGEO_ST& operator[](const size_t& row);
  const DCGEO_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_ncells(size_t n, short v) {
    fTableData[n].ncells = v;
  }
  short get_ncells(size_t n) const {
    return fTableData[n].ncells;
  }
  void set_ngusset(size_t n, short v) {
    fTableData[n].ngusset = v;
  }
  short get_ngusset(size_t n) const {
    return fTableData[n].ngusset;
  }
  void set_ti_switch(size_t n, short v) {
    fTableData[n].ti_switch = v;
  }
  short get_ti_switch(size_t n) const {
    return fTableData[n].ti_switch;
  }
  void set_suppzlength(size_t n, float v) {
    fTableData[n].suppzlength = v;
  }
  float get_suppzlength(size_t n) const {
    return fTableData[n].suppzlength;
  }
  void set_inradius(size_t n, float v) {
    fTableData[n].inradius = v;
  }
  float get_inradius(size_t n) const {
    return fTableData[n].inradius;
  }
  void set_outradius(size_t n, float v) {
    fTableData[n].outradius = v;
  }
  float get_outradius(size_t n) const {
    return fTableData[n].outradius;
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
  void set_phitope(size_t n, float v) {
    fTableData[n].phitope = v;
  }
  float get_phitope(size_t n) const {
    return fTableData[n].phitope;
  }
  void set_phibote(size_t n, float v) {
    fTableData[n].phibote = v;
  }
  float get_phibote(size_t n) const {
    return fTableData[n].phibote;
  }
  void set_rplane(size_t d0, size_t n, float v) {
    fTableData[n].rplane[d0] = v;
  }
  float get_rplane(size_t d0, size_t n) const {
    return fTableData[n].rplane[d0];
  }
  void set_planethick(size_t n, float v) {
    fTableData[n].planethick = v;
  }
  float get_planethick(size_t n) const {
    return fTableData[n].planethick;
  }
  void set_uvangle(size_t n, float v) {
    fTableData[n].uvangle = v;
  }
  float get_uvangle(size_t n) const {
    return fTableData[n].uvangle;
  }
  void set_winthickin(size_t n, float v) {
    fTableData[n].winthickin = v;
  }
  float get_winthickin(size_t n) const {
    return fTableData[n].winthickin;
  }
  void set_winthickout(size_t n, float v) {
    fTableData[n].winthickout = v;
  }
  float get_winthickout(size_t n) const {
    return fTableData[n].winthickout;
  }
  void set_supptiside(size_t n, float v) {
    fTableData[n].supptiside = v;
  }
  float get_supptiside(size_t n) const {
    return fTableData[n].supptiside;
  }
  void set_suppalside(size_t n, float v) {
    fTableData[n].suppalside = v;
  }
  float get_suppalside(size_t n) const {
    return fTableData[n].suppalside;
  }
  void set_suppzthick(size_t n, float v) {
    fTableData[n].suppzthick = v;
  }
  float get_suppzthick(size_t n) const {
    return fTableData[n].suppzthick;
  }
  void set_supptibase(size_t n, float v) {
    fTableData[n].supptibase = v;
  }
  float get_supptibase(size_t n) const {
    return fTableData[n].supptibase;
  }
  void set_suppalbase(size_t n, float v) {
    fTableData[n].suppalbase = v;
  }
  float get_suppalbase(size_t n) const {
    return fTableData[n].suppalbase;
  }
  void set_x1baserad(size_t n, float v) {
    fTableData[n].x1baserad = v;
  }
  float get_x1baserad(size_t n) const {
    return fTableData[n].x1baserad;
  }
  void set_x2baserad(size_t n, float v) {
    fTableData[n].x2baserad = v;
  }
  float get_x2baserad(size_t n) const {
    return fTableData[n].x2baserad;
  }
  void set_x1basez(size_t n, float v) {
    fTableData[n].x1basez = v;
  }
  float get_x1basez(size_t n) const {
    return fTableData[n].x1basez;
  }
  void set_x2basez(size_t n, float v) {
    fTableData[n].x2basez = v;
  }
  float get_x2basez(size_t n) const {
    return fTableData[n].x2basez;
  }
  void set_x1slotthick(size_t n, float v) {
    fTableData[n].x1slotthick = v;
  }
  float get_x1slotthick(size_t n) const {
    return fTableData[n].x1slotthick;
  }
  void set_x2slotthick(size_t n, float v) {
    fTableData[n].x2slotthick = v;
  }
  float get_x2slotthick(size_t n) const {
    return fTableData[n].x2slotthick;
  }
  void set_x1slotz(size_t n, float v) {
    fTableData[n].x1slotz = v;
  }
  float get_x1slotz(size_t n) const {
    return fTableData[n].x1slotz;
  }
  void set_x2slotz(size_t n, float v) {
    fTableData[n].x2slotz = v;
  }
  float get_x2slotz(size_t n) const {
    return fTableData[n].x2slotz;
  }
  void set_x1suppthick(size_t n, float v) {
    fTableData[n].x1suppthick = v;
  }
  float get_x1suppthick(size_t n) const {
    return fTableData[n].x1suppthick;
  }
  void set_x2suppthick(size_t n, float v) {
    fTableData[n].x2suppthick = v;
  }
  float get_x2suppthick(size_t n) const {
    return fTableData[n].x2suppthick;
  }
  void set_x1suppz(size_t n, float v) {
    fTableData[n].x1suppz = v;
  }
  float get_x1suppz(size_t n) const {
    return fTableData[n].x1suppz;
  }
  void set_x2suppz(size_t n, float v) {
    fTableData[n].x2suppz = v;
  }
  float get_x2suppz(size_t n) const {
    return fTableData[n].x2suppz;
  }
  void set_x1rextent(size_t n, float v) {
    fTableData[n].x1rextent = v;
  }
  float get_x1rextent(size_t n) const {
    return fTableData[n].x1rextent;
  }
  void set_x2rextent(size_t n, float v) {
    fTableData[n].x2rextent = v;
  }
  float get_x2rextent(size_t n) const {
    return fTableData[n].x2rextent;
  }
  void set_u1rextent(size_t n, float v) {
    fTableData[n].u1rextent = v;
  }
  float get_u1rextent(size_t n) const {
    return fTableData[n].u1rextent;
  }
  void set_v1rextent(size_t n, float v) {
    fTableData[n].v1rextent = v;
  }
  float get_v1rextent(size_t n) const {
    return fTableData[n].v1rextent;
  }
  void set_u2rextent(size_t n, float v) {
    fTableData[n].u2rextent = v;
  }
  float get_u2rextent(size_t n) const {
    return fTableData[n].u2rextent;
  }
  void set_v2rextent(size_t n, float v) {
    fTableData[n].v2rextent = v;
  }
  float get_v2rextent(size_t n) const {
    return fTableData[n].v2rextent;
  }
  void set_u1basez(size_t n, float v) {
    fTableData[n].u1basez = v;
  }
  float get_u1basez(size_t n) const {
    return fTableData[n].u1basez;
  }
  void set_v1basez(size_t n, float v) {
    fTableData[n].v1basez = v;
  }
  float get_v1basez(size_t n) const {
    return fTableData[n].v1basez;
  }
  void set_u2basez(size_t n, float v) {
    fTableData[n].u2basez = v;
  }
  float get_u2basez(size_t n) const {
    return fTableData[n].u2basez;
  }
  void set_v2basez(size_t n, float v) {
    fTableData[n].v2basez = v;
  }
  float get_v2basez(size_t n) const {
    return fTableData[n].v2basez;
  }
  void set_u1slotz(size_t n, float v) {
    fTableData[n].u1slotz = v;
  }
  float get_u1slotz(size_t n) const {
    return fTableData[n].u1slotz;
  }
  void set_v1slotz(size_t n, float v) {
    fTableData[n].v1slotz = v;
  }
  float get_v1slotz(size_t n) const {
    return fTableData[n].v1slotz;
  }
  void set_u2slotz(size_t n, float v) {
    fTableData[n].u2slotz = v;
  }
  float get_u2slotz(size_t n) const {
    return fTableData[n].u2slotz;
  }
  void set_v2slotz(size_t n, float v) {
    fTableData[n].v2slotz = v;
  }
  float get_v2slotz(size_t n) const {
    return fTableData[n].v2slotz;
  }
  void set_u1suppz(size_t n, float v) {
    fTableData[n].u1suppz = v;
  }
  float get_u1suppz(size_t n) const {
    return fTableData[n].u1suppz;
  }
  void set_v1suppz(size_t n, float v) {
    fTableData[n].v1suppz = v;
  }
  float get_v1suppz(size_t n) const {
    return fTableData[n].v1suppz;
  }
  void set_u2suppz(size_t n, float v) {
    fTableData[n].u2suppz = v;
  }
  float get_u2suppz(size_t n) const {
    return fTableData[n].u2suppz;
  }
  void set_v2suppz(size_t n, float v) {
    fTableData[n].v2suppz = v;
  }
  float get_v2suppz(size_t n) const {
    return fTableData[n].v2suppz;
  }
  void set_cfibinrad(size_t n, float v) {
    fTableData[n].cfibinrad = v;
  }
  float get_cfibinrad(size_t n) const {
    return fTableData[n].cfibinrad;
  }
  void set_cfiboutrad(size_t n, float v) {
    fTableData[n].cfiboutrad = v;
  }
  float get_cfiboutrad(size_t n) const {
    return fTableData[n].cfiboutrad;
  }

private:
  DCGEO_ST* fTableData;

  ClassDef(dcgeoWrapper,1)
};
#endif /*__DCGEOWRAPPER_H__*/
