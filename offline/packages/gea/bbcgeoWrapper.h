#ifndef __BBCGEOWRAPPER_H__
#define __BBCGEOWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "bbcgeo.h"
class bbcgeoWrapper: public PHTable
{
public:
  bbcgeoWrapper(const char* name = "bbcgeo", const size_t& max_rows = 1);
//  bbcgeoWrapper(const bbcgeoWrapper& source);
//  bbcgeoWrapper& operator=(const bbcgeoWrapper& source);

  ~bbcgeoWrapper();

  void* RawTableData();
  BBCGEO_ST* TableData();

  BBCGEO_ST& operator[](const size_t& row);
  const BBCGEO_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_absorb(size_t d0, size_t n, float v) {
    fTableData[n].absorb[d0] = v;
  }
  float get_absorb(size_t d0, size_t n) const {
    return fTableData[n].absorb[d0];
  }
  void set_attach(size_t d0, size_t n, float v) {
    fTableData[n].attach[d0] = v;
  }
  float get_attach(size_t d0, size_t n) const {
    return fTableData[n].attach[d0];
  }
  void set_backbd(size_t d0, size_t n, float v) {
    fTableData[n].backbd[d0] = v;
  }
  float get_backbd(size_t d0, size_t n) const {
    return fTableData[n].backbd[d0];
  }
  void set_covert(size_t n, float v) {
    fTableData[n].covert = v;
  }
  float get_covert(size_t n) const {
    return fTableData[n].covert;
  }
  void set_frontb(size_t d0, size_t n, float v) {
    fTableData[n].frontb[d0] = v;
  }
  float get_frontb(size_t d0, size_t n) const {
    return fTableData[n].frontb[d0];
  }
  void set_pmtsiz(size_t d0, size_t n, float v) {
    fTableData[n].pmtsiz[d0] = v;
  }
  float get_pmtsiz(size_t d0, size_t n) const {
    return fTableData[n].pmtsiz[d0];
  }
  void set_quartz(size_t d0, size_t n, float v) {
    fTableData[n].quartz[d0] = v;
  }
  float get_quartz(size_t d0, size_t n) const {
    return fTableData[n].quartz[d0];
  }
  void set_spacin(size_t n, float v) {
    fTableData[n].spacin = v;
  }
  float get_spacin(size_t n) const {
    return fTableData[n].spacin;
  }
  void set_struc(size_t d0, size_t n, float v) {
    fTableData[n].struc[d0] = v;
  }
  float get_struc(size_t d0, size_t n) const {
    return fTableData[n].struc[d0];
  }
  void set_zposit(size_t d0, size_t n, float v) {
    fTableData[n].zposit[d0] = v;
  }
  float get_zposit(size_t d0, size_t n) const {
    return fTableData[n].zposit[d0];
  }
  void set_color(size_t n, short v) {
    fTableData[n].color = v;
  }
  short get_color(size_t n) const {
    return fTableData[n].color;
  }
  void set_seen(size_t n, short v) {
    fTableData[n].seen = v;
  }
  short get_seen(size_t n) const {
    return fTableData[n].seen;
  }
  void set_medabs(size_t n, short v) {
    fTableData[n].medabs = v;
  }
  short get_medabs(size_t n) const {
    return fTableData[n].medabs;
  }
  void set_medatt(size_t n, short v) {
    fTableData[n].medatt = v;
  }
  short get_medatt(size_t n) const {
    return fTableData[n].medatt;
  }
  void set_medbac(size_t n, short v) {
    fTableData[n].medbac = v;
  }
  short get_medbac(size_t n) const {
    return fTableData[n].medbac;
  }
  void set_medcov(size_t n, short v) {
    fTableData[n].medcov = v;
  }
  short get_medcov(size_t n) const {
    return fTableData[n].medcov;
  }
  void set_medfro(size_t n, short v) {
    fTableData[n].medfro = v;
  }
  short get_medfro(size_t n) const {
    return fTableData[n].medfro;
  }
  void set_medmot(size_t n, short v) {
    fTableData[n].medmot = v;
  }
  short get_medmot(size_t n) const {
    return fTableData[n].medmot;
  }
  void set_medpmt(size_t n, short v) {
    fTableData[n].medpmt = v;
  }
  short get_medpmt(size_t n) const {
    return fTableData[n].medpmt;
  }
  void set_medqua(size_t n, short v) {
    fTableData[n].medqua = v;
  }
  short get_medqua(size_t n) const {
    return fTableData[n].medqua;
  }
  void set_medstr(size_t n, short v) {
    fTableData[n].medstr = v;
  }
  short get_medstr(size_t n) const {
    return fTableData[n].medstr;
  }

private:
  BBCGEO_ST* fTableData;

  ClassDef(bbcgeoWrapper,1)
};
#endif /*__BBCGEOWRAPPER_H__*/
