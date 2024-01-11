#ifndef __DEMCGEATRACKCLUSTERWRAPPER_H__
#define __DEMCGEATRACKCLUSTERWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcGeaTrackCluster.h"
class dEmcGeaTrackClusterWrapper: public PHTable
{
public:
  dEmcGeaTrackClusterWrapper(const char* name = "dEmcGeaTrackCluster", const size_t& max_rows = 1);
//  dEmcGeaTrackClusterWrapper(const dEmcGeaTrackClusterWrapper& source);
//  dEmcGeaTrackClusterWrapper& operator=(const dEmcGeaTrackClusterWrapper& source);

  ~dEmcGeaTrackClusterWrapper();

  void* RawTableData();
  DEMCGEATRACKCLUSTER_ST* TableData();

  DEMCGEATRACKCLUSTER_ST& operator[](const size_t& row);
  const DEMCGEATRACKCLUSTER_ST& operator[](const size_t& row) const;

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
  void set_track_ptr(size_t n, int v) {
    fTableData[n].track_ptr = v;
  }
  int get_track_ptr(size_t n) const {
    return fTableData[n].track_ptr;
  }
  void set_input(size_t n, short v) {
    fTableData[n].input = v;
  }
  short get_input(size_t n) const {
    return fTableData[n].input;
  }
  void set_clusid(size_t d0, size_t n, int v) {
    fTableData[n].clusid[d0] = v;
  }
  int get_clusid(size_t d0, size_t n) const {
    return fTableData[n].clusid[d0];
  }
  void set_pid(size_t n, float v) {
    fTableData[n].pid = v;
  }
  float get_pid(size_t n) const {
    return fTableData[n].pid;
  }
  void set_ptot(size_t n, float v) {
    fTableData[n].ptot = v;
  }
  float get_ptot(size_t n) const {
    return fTableData[n].ptot;
  }
  void set_nom_edep(size_t n, float v) {
    fTableData[n].nom_edep = v;
  }
  float get_nom_edep(size_t n) const {
    return fTableData[n].nom_edep;
  }
  void set_edep(size_t d0, size_t n, float v) {
    fTableData[n].edep[d0] = v;
  }
  float get_edep(size_t d0, size_t n) const {
    return fTableData[n].edep[d0];
  }
  void set_efrac(size_t d0, size_t n, float v) {
    fTableData[n].efrac[d0] = v;
  }
  float get_efrac(size_t d0, size_t n) const {
    return fTableData[n].efrac[d0];
  }

private:
  DEMCGEATRACKCLUSTER_ST* fTableData;

  ClassDef(dEmcGeaTrackClusterWrapper,1)
};
#endif /*__DEMCGEATRACKCLUSTERWRAPPER_H__*/
