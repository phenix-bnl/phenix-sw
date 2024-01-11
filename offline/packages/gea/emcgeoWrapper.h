#ifndef __EMCGEOWRAPPER_H__
#define __EMCGEOWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "emcgeo.h"
class emcgeoWrapper: public PHTable
{
public:
  emcgeoWrapper(const char* name = "emcgeo", const size_t& max_rows = 1);
//  emcgeoWrapper(const emcgeoWrapper& source);
//  emcgeoWrapper& operator=(const emcgeoWrapper& source);

  ~emcgeoWrapper();

  void* RawTableData();
  EMCGEO_ST* TableData();

  EMCGEO_ST& operator[](const size_t& row);
  const EMCGEO_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_walls(size_t n, float v) {
    fTableData[n].walls = v;
  }
  float get_walls(size_t n) const {
    return fTableData[n].walls;
  }
  void set_opt(size_t n, float v) {
    fTableData[n].opt = v;
  }
  float get_opt(size_t n) const {
    return fTableData[n].opt;
  }
  void set_angle(size_t n, float v) {
    fTableData[n].angle = v;
  }
  float get_angle(size_t n) const {
    return fTableData[n].angle;
  }
  void set_rpos(size_t n, float v) {
    fTableData[n].rpos = v;
  }
  float get_rpos(size_t n) const {
    return fTableData[n].rpos;
  }

private:
  EMCGEO_ST* fTableData;

  ClassDef(emcgeoWrapper,1)
};
#endif /*__EMCGEOWRAPPER_H__*/
