#ifndef __DEMCGEATOWERTRACKWRAPPER_H__
#define __DEMCGEATOWERTRACKWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcGeaTowerTrack.h"
class dEmcGeaTowerTrackWrapper: public PHTable
{
public:
  dEmcGeaTowerTrackWrapper(const char* name = "dEmcGeaTowerTrack", const size_t& max_rows = 1);
//  dEmcGeaTowerTrackWrapper(const dEmcGeaTowerTrackWrapper& source);
//  dEmcGeaTowerTrackWrapper& operator=(const dEmcGeaTowerTrackWrapper& source);

  ~dEmcGeaTowerTrackWrapper();

  void* RawTableData();
  DEMCGEATOWERTRACK_ST* TableData();

  DEMCGEATOWERTRACK_ST& operator[](const size_t& row);
  const DEMCGEATOWERTRACK_ST& operator[](const size_t& row) const;

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
  void set_input(size_t n, short v) {
    fTableData[n].input = v;
  }
  short get_input(size_t n) const {
    return fTableData[n].input;
  }
  void set_trkno(size_t d0, size_t n, int v) {
    fTableData[n].trkno[d0] = v;
  }
  int get_trkno(size_t d0, size_t n) const {
    return fTableData[n].trkno[d0];
  }
  void set_edep(size_t d0, size_t n, float v) {
    fTableData[n].edep[d0] = v;
  }
  float get_edep(size_t d0, size_t n) const {
    return fTableData[n].edep[d0];
  }
  void set_toffirst(size_t d0, size_t n, float v) {
    fTableData[n].toffirst[d0] = v;
  }
  float get_toffirst(size_t d0, size_t n) const {
    return fTableData[n].toffirst[d0];
  }

private:
  DEMCGEATOWERTRACK_ST* fTableData;

  ClassDef(dEmcGeaTowerTrackWrapper,1)
};
#endif /*__DEMCGEATOWERTRACKWRAPPER_H__*/
