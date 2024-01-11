#ifndef __DCRKPROJPIDPARWRAPPER_H__
#define __DCRKPROJPIDPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkProjPidPar.h"
class dCrkProjPidParWrapper: public PHTable
{
public:
  dCrkProjPidParWrapper(const char* name = "dCrkProjPidPar", const size_t& max_rows = 1);
//  dCrkProjPidParWrapper(const dCrkProjPidParWrapper& source);
//  dCrkProjPidParWrapper& operator=(const dCrkProjPidParWrapper& source);

  ~dCrkProjPidParWrapper();

  void* RawTableData();
  DCRKPROJPIDPAR_ST* TableData();

  DCRKPROJPIDPAR_ST& operator[](const size_t& row);
  const DCRKPROJPIDPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_in_accept(size_t n, short v) {
    fTableData[n].in_accept = v;
  }
  short get_in_accept(size_t n) const {
    return fTableData[n].in_accept;
  }
  void set_min_npmt(size_t n, short v) {
    fTableData[n].min_npmt = v;
  }
  short get_min_npmt(size_t n) const {
    return fTableData[n].min_npmt;
  }
  void set_gas(size_t n, short v) {
    fTableData[n].gas = v;
  }
  short get_gas(size_t n) const {
    return fTableData[n].gas;
  }
  void set_Rmax(size_t n, float v) {
    fTableData[n].Rmax = v;
  }
  float get_Rmax(size_t n) const {
    return fTableData[n].Rmax;
  }
  void set_Rmin(size_t n, float v) {
    fTableData[n].Rmin = v;
  }
  float get_Rmin(size_t n) const {
    return fTableData[n].Rmin;
  }
  void set_Rmax2(size_t n, float v) {
    fTableData[n].Rmax2 = v;
  }
  float get_Rmax2(size_t n) const {
    return fTableData[n].Rmax2;
  }
  void set_R0(size_t n, float v) {
    fTableData[n].R0 = v;
  }
  float get_R0(size_t n) const {
    return fTableData[n].R0;
  }

private:
  DCRKPROJPIDPAR_ST* fTableData;

  ClassDef(dCrkProjPidParWrapper,1)
};
#endif /*__DCRKPROJPIDPARWRAPPER_H__*/
