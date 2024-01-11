#ifndef __DTECGHITDCMWRAPPER_H__
#define __DTECGHITDCMWRAPPER_H__

#include <cstddef>
#include "PHTable.hh"
#include "dTecGhitDcm.h"

class dTecGhitDcmWrapper: public PHTable
{
public:
  dTecGhitDcmWrapper(const char* name = "dTecGhitDcm", const size_t& max_rows = 1);
//  dTecGhitDcmWrapper(const dTecGhitDcmWrapper& source);
//  dTecGhitDcmWrapper& operator=(const dTecGhitDcmWrapper& source);

  ~dTecGhitDcmWrapper();

  void* RawTableData();
  DTECGHITDCM_ST* TableData();

  DTECGHITDCM_ST& operator[](const size_t& row);
  const DTECGHITDCM_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_ghitid(size_t n, short v) {
    fTableData[n].ghitid = v;
  }
  short get_ghitid(size_t n) const {
    return fTableData[n].ghitid;
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
  void set_side(size_t n, short v) {
    fTableData[n].side = v;
  }
  short get_side(size_t n) const {
    return fTableData[n].side;
  }
  void set_wire(size_t n, short v) {
    fTableData[n].wire = v;
  }
  short get_wire(size_t n) const {
    return fTableData[n].wire;
  }
  void set_crate(size_t n, short v) {
    fTableData[n].crate = v;
  }
  short get_crate(size_t n) const {
    return fTableData[n].crate;
  }
  void set_slot(size_t n, short v) {
    fTableData[n].slot = v;
  }
  short get_slot(size_t n) const {
    return fTableData[n].slot;
  }
  void set_channel(size_t n, short v) {
    fTableData[n].channel = v;
  }
  short get_channel(size_t n) const {
    return fTableData[n].channel;
  }
  void set_binnum(size_t n, short v) {
    fTableData[n].binnum = v;
  }
  short get_binnum(size_t n) const {
    return fTableData[n].binnum;
  }
  void set_fraction(size_t n, float v) {
    fTableData[n].fraction = v;
  }
  float get_fraction(size_t n) const {
    return fTableData[n].fraction;
  }

private:
  DTECGHITDCM_ST* fTableData;

  ClassDef(dTecGhitDcmWrapper,1)
};
#endif /*__DTECGHITDCMWRAPPER_H__*/
