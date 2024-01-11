#ifndef __DCRKHITWRAPPER_H__
#define __DCRKHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkHit.h"
class dCrkHitWrapper: public PHTable
{
public:
  dCrkHitWrapper(const char* name = "dCrkHit", const size_t& max_rows = 1);
//  dCrkHitWrapper(const dCrkHitWrapper& source);
//  dCrkHitWrapper& operator=(const dCrkHitWrapper& source);

  ~dCrkHitWrapper();

  void* RawTableData();
  DCRKHIT_ST* TableData();

  DCRKHIT_ST& operator[](const size_t& row);
  const DCRKHIT_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_pmt(size_t n, short v) {
    fTableData[n].pmt = v;
  }
  short get_pmt(size_t n) const {
    return fTableData[n].pmt;
  }
  void set_npe(size_t n, float v) {
    fTableData[n].npe = v;
  }
  float get_npe(size_t n) const {
    return fTableData[n].npe;
  }
  void set_time(size_t n, float v) {
    fTableData[n].time = v;
  }
  float get_time(size_t n) const {
    return fTableData[n].time;
  }

private:
  DCRKHIT_ST* fTableData;

  ClassDef(dCrkHitWrapper,1)
};
#endif /*__DCRKHITWRAPPER_H__*/
