#ifndef __DEMCGEATRACKTOWERWRAPPER_H__
#define __DEMCGEATRACKTOWERWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcGeaTrackTower.h"
class dEmcGeaTrackTowerWrapper: public PHTable
{
public:
  dEmcGeaTrackTowerWrapper(const char* name = "dEmcGeaTrackTower", const size_t& max_rows = 1);
//  dEmcGeaTrackTowerWrapper(const dEmcGeaTrackTowerWrapper& source);
//  dEmcGeaTrackTowerWrapper& operator=(const dEmcGeaTrackTowerWrapper& source);

  ~dEmcGeaTrackTowerWrapper();

  void* RawTableData();
  DEMCGEATRACKTOWER_ST* TableData();

  DEMCGEATRACKTOWER_ST& operator[](const size_t& row);
  const DEMCGEATRACKTOWER_ST& operator[](const size_t& row) const;

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
  void set_input(size_t n, short v) {
    fTableData[n].input = v;
  }
  short get_input(size_t n) const {
    return fTableData[n].input;
  }
  void set_xyz(size_t d0, size_t n, float v) {
    fTableData[n].xyz[d0] = v;
  }
  float get_xyz(size_t d0, size_t n) const {
    return fTableData[n].xyz[d0];
  }
  void set_twrkey(size_t d0, size_t n, int v) {
    fTableData[n].twrkey[d0] = v;
  }
  int get_twrkey(size_t d0, size_t n) const {
    return fTableData[n].twrkey[d0];
  }
  void set_edep(size_t d0, size_t n, float v) {
    fTableData[n].edep[d0] = v;
  }
  float get_edep(size_t d0, size_t n) const {
    return fTableData[n].edep[d0];
  }
  void set_nextid(size_t n, int v) {
    fTableData[n].nextid = v;
  }
  int get_nextid(size_t n) const {
    return fTableData[n].nextid;
  }

private:
  DEMCGEATRACKTOWER_ST* fTableData;

  ClassDef(dEmcGeaTrackTowerWrapper,1)
};
#endif /*__DEMCGEATRACKTOWERWRAPPER_H__*/
