#ifndef __DEMCGEATOWEREVALWRAPPER_H__
#define __DEMCGEATOWEREVALWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcGeaTowerEval.h"
class dEmcGeaTowerEvalWrapper: public PHTable
{
public:
  dEmcGeaTowerEvalWrapper(const char* name = "dEmcGeaTowerEval", const size_t& max_rows = 1);
//  dEmcGeaTowerEvalWrapper(const dEmcGeaTowerEvalWrapper& source);
//  dEmcGeaTowerEvalWrapper& operator=(const dEmcGeaTowerEvalWrapper& source);

  ~dEmcGeaTowerEvalWrapper();

  void* RawTableData();
  DEMCGEATOWEREVAL_ST* TableData();

  DEMCGEATOWEREVAL_ST& operator[](const size_t& row);
  const DEMCGEATOWEREVAL_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, int v) {
    fTableData[n].id = v;
  }
  int get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_twrkey(size_t n, int v) {
    fTableData[n].twrkey = v;
  }
  int get_twrkey(size_t n) const {
    return fTableData[n].twrkey;
  }
  void set_input(size_t n, int v) {
    fTableData[n].input = v;
  }
  int get_input(size_t n) const {
    return fTableData[n].input;
  }
  void set_arm(size_t n, int v) {
    fTableData[n].arm = v;
  }
  int get_arm(size_t n) const {
    return fTableData[n].arm;
  }
  void set_sector(size_t n, int v) {
    fTableData[n].sector = v;
  }
  int get_sector(size_t n) const {
    return fTableData[n].sector;
  }
  void set_indy(size_t n, int v) {
    fTableData[n].indy = v;
  }
  int get_indy(size_t n) const {
    return fTableData[n].indy;
  }
  void set_indz(size_t n, int v) {
    fTableData[n].indz = v;
  }
  int get_indz(size_t n) const {
    return fTableData[n].indz;
  }
  void set_trkno(size_t d0, size_t n, int v) {
    fTableData[n].trkno[d0] = v;
  }
  int get_trkno(size_t d0, size_t n) const {
    return fTableData[n].trkno[d0];
  }
  void set_pid(size_t d0, size_t n, float v) {
    fTableData[n].pid[d0] = v;
  }
  float get_pid(size_t d0, size_t n) const {
    return fTableData[n].pid[d0];
  }
  void set_ptot(size_t d0, size_t n, float v) {
    fTableData[n].ptot[d0] = v;
  }
  float get_ptot(size_t d0, size_t n) const {
    return fTableData[n].ptot[d0];
  }
  void set_vertex(size_t d0, size_t d1, size_t n, float v) {
    fTableData[n].vertex[d0][d1] = v;
  }
  float get_vertex(size_t d0, size_t d1, size_t n) const {
    return fTableData[n].vertex[d0][d1];
  }
  void set_ancestry(size_t d0, size_t n, float v) {
    fTableData[n].ancestry[d0] = v;
  }
  float get_ancestry(size_t d0, size_t n) const {
    return fTableData[n].ancestry[d0];
  }
  void set_itparent(size_t d0, size_t n, int v) {
    fTableData[n].itparent[d0] = v;
  }
  int get_itparent(size_t d0, size_t n) const {
    return fTableData[n].itparent[d0];
  }
  void set_idparent(size_t d0, size_t n, int v) {
    fTableData[n].idparent[d0] = v;
  }
  int get_idparent(size_t d0, size_t n) const {
    return fTableData[n].idparent[d0];
  }
  void set_edep(size_t d0, size_t n, float v) {
    fTableData[n].edep[d0] = v;
  }
  float get_edep(size_t d0, size_t n) const {
    return fTableData[n].edep[d0];
  }
  void set_mease(size_t n, float v) {
    fTableData[n].mease = v;
  }
  float get_mease(size_t n) const {
    return fTableData[n].mease;
  }
  void set_tof(size_t n, float v) {
    fTableData[n].tof = v;
  }
  float get_tof(size_t n) const {
    return fTableData[n].tof;
  }
  void set_toffirst(size_t d0, size_t n, float v) {
    fTableData[n].toffirst[d0] = v;
  }
  float get_toffirst(size_t d0, size_t n) const {
    return fTableData[n].toffirst[d0];
  }
  void set_dist_nom(size_t n, float v) {
    fTableData[n].dist_nom = v;
  }
  float get_dist_nom(size_t n) const {
    return fTableData[n].dist_nom;
  }
  void set_dist_act(size_t n, float v) {
    fTableData[n].dist_act = v;
  }
  float get_dist_act(size_t n) const {
    return fTableData[n].dist_act;
  }
  void set_sinthe_nom(size_t n, float v) {
    fTableData[n].sinthe_nom = v;
  }
  float get_sinthe_nom(size_t n) const {
    return fTableData[n].sinthe_nom;
  }
  void set_sinthe_act(size_t n, float v) {
    fTableData[n].sinthe_act = v;
  }
  float get_sinthe_act(size_t n) const {
    return fTableData[n].sinthe_act;
  }

private:
  DEMCGEATOWEREVAL_ST* fTableData;

  ClassDef(dEmcGeaTowerEvalWrapper,1)
};
#endif /*__DEMCGEATOWEREVALWRAPPER_H__*/
