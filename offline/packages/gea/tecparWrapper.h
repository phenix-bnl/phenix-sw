#ifndef __TECPARWRAPPER_H__
#define __TECPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "tecpar.h"
class tecparWrapper: public PHTable
{
public:
  tecparWrapper(const char* name = "tecpar", const size_t& max_rows = 1);
//  tecparWrapper(const tecparWrapper& source);
//  tecparWrapper& operator=(const tecparWrapper& source);

  ~tecparWrapper();

  void* RawTableData();
  TECPAR_ST* TableData();

  TECPAR_ST& operator[](const size_t& row);
  const TECPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_r0(size_t n, float v) {
    fTableData[n].r0 = v;
  }
  float get_r0(size_t n) const {
    return fTableData[n].r0;
  }
  void set_angle(size_t n, float v) {
    fTableData[n].angle = v;
  }
  float get_angle(size_t n) const {
    return fTableData[n].angle;
  }
  void set_thRad(size_t n, float v) {
    fTableData[n].thRad = v;
  }
  float get_thRad(size_t n) const {
    return fTableData[n].thRad;
  }
  void set_thXe(size_t n, float v) {
    fTableData[n].thXe = v;
  }
  float get_thXe(size_t n) const {
    return fTableData[n].thXe;
  }
  void set_nArms(size_t n, short v) {
    fTableData[n].nArms = v;
  }
  short get_nArms(size_t n) const {
    return fTableData[n].nArms;
  }
  void set_nSect(size_t n, short v) {
    fTableData[n].nSect = v;
  }
  short get_nSect(size_t n) const {
    return fTableData[n].nSect;
  }
  void set_iArmor(size_t n, short v) {
    fTableData[n].iArmor = v;
  }
  short get_iArmor(size_t n) const {
    return fTableData[n].iArmor;
  }
  void set_NTEC(size_t n, short v) {
    fTableData[n].NTEC = v;
  }
  short get_NTEC(size_t n) const {
    return fTableData[n].NTEC;
  }
  void set_lTec(size_t d0, size_t n, short v) {
    fTableData[n].lTec[d0] = v;
  }
  short get_lTec(size_t d0, size_t n) const {
    return fTableData[n].lTec[d0];
  }
  void set_iDateTEC(size_t n, int v) {
    fTableData[n].iDateTEC = v;
  }
  int get_iDateTEC(size_t n) const {
    return fTableData[n].iDateTEC;
  }

private:
  TECPAR_ST* fTableData;

  ClassDef(tecparWrapper,1)
};
#endif /*__TECPARWRAPPER_H__*/
