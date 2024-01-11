#ifndef __DPADCLUSTERWRAPPER_H__
#define __DPADCLUSTERWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadCluster.h"
class dPadClusterWrapper: public PHTable
{
public:
  dPadClusterWrapper(const char* name = "dPadCluster", const size_t& max_rows = 1);
//  dPadClusterWrapper(const dPadClusterWrapper& source);
//  dPadClusterWrapper& operator=(const dPadClusterWrapper& source);

  ~dPadClusterWrapper();

  void* RawTableData();
  DPADCLUSTER_ST* TableData();

  DPADCLUSTER_ST& operator[](const size_t& row);
  const DPADCLUSTER_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
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
  void set_wire(size_t n, short v) {
    fTableData[n].wire = v;
  }
  short get_wire(size_t n) const {
    return fTableData[n].wire;
  }
  void set_cell(size_t n, short v) {
    fTableData[n].cell = v;
  }
  short get_cell(size_t n) const {
    return fTableData[n].cell;
  }
  void set_xyz(size_t d0, size_t n, float v) {
    fTableData[n].xyz[d0] = v;
  }
  float get_xyz(size_t d0, size_t n) const {
    return fTableData[n].xyz[d0];
  }
  void set_dxyz(size_t d0, size_t n, float v) {
    fTableData[n].dxyz[d0] = v;
  }
  float get_dxyz(size_t d0, size_t n) const {
    return fTableData[n].dxyz[d0];
  }
  void set_type(size_t n, short v) {
    fTableData[n].type = v;
  }
  short get_type(size_t n) const {
    return fTableData[n].type;
  }

private:
  DPADCLUSTER_ST* fTableData;

  ClassDef(dPadClusterWrapper,1)
};
#endif /*__DPADCLUSTERWRAPPER_H__*/
