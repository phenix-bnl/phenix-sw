#ifndef __TOFPARAWRAPPER_H__
#define __TOFPARAWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "tofpara.h"
class tofparaWrapper: public PHTable
{
public:
  tofparaWrapper(const char* name = "tofpara", const size_t& max_rows = 1);
//  tofparaWrapper(const tofparaWrapper& source);
//  tofparaWrapper& operator=(const tofparaWrapper& source);

  ~tofparaWrapper();

  void* RawTableData();
  TOFPARA_ST* TableData();

  TOFPARA_ST& operator[](const size_t& row);
  const TOFPARA_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_tofl_rpos(size_t n, float v) {
    fTableData[n].tofl_rpos = v;
  }
  float get_tofl_rpos(size_t n) const {
    return fTableData[n].tofl_rpos;
  }
  void set_tfsp_phi_1(size_t n, float v) {
    fTableData[n].tfsp_phi_1 = v;
  }
  float get_tfsp_phi_1(size_t n) const {
    return fTableData[n].tfsp_phi_1;
  }
  void set_tfsp_phi_2(size_t n, float v) {
    fTableData[n].tfsp_phi_2 = v;
  }
  float get_tfsp_phi_2(size_t n) const {
    return fTableData[n].tfsp_phi_2;
  }
  void set_tfsp_dimen(size_t d0, size_t n, float v) {
    fTableData[n].tfsp_dimen[d0] = v;
  }
  float get_tfsp_dimen(size_t d0, size_t n) const {
    return fTableData[n].tfsp_dimen[d0];
  }
  void set_tfsp_nslat(size_t n, int v) {
    fTableData[n].tfsp_nslat = v;
  }
  int get_tfsp_nslat(size_t n) const {
    return fTableData[n].tfsp_nslat;
  }
  void set_tfsp_isegm(size_t n, int v) {
    fTableData[n].tfsp_isegm = v;
  }
  int get_tfsp_isegm(size_t n) const {
    return fTableData[n].tfsp_isegm;
  }
  void set_tflp_phi_1(size_t n, float v) {
    fTableData[n].tflp_phi_1 = v;
  }
  float get_tflp_phi_1(size_t n) const {
    return fTableData[n].tflp_phi_1;
  }
  void set_tflp_phi_2(size_t n, float v) {
    fTableData[n].tflp_phi_2 = v;
  }
  float get_tflp_phi_2(size_t n) const {
    return fTableData[n].tflp_phi_2;
  }
  void set_tflp_phi_3(size_t n, float v) {
    fTableData[n].tflp_phi_3 = v;
  }
  float get_tflp_phi_3(size_t n) const {
    return fTableData[n].tflp_phi_3;
  }
  void set_tflp_phi_4(size_t n, float v) {
    fTableData[n].tflp_phi_4 = v;
  }
  float get_tflp_phi_4(size_t n) const {
    return fTableData[n].tflp_phi_4;
  }
  void set_tflp_dimen(size_t d0, size_t n, float v) {
    fTableData[n].tflp_dimen[d0] = v;
  }
  float get_tflp_dimen(size_t d0, size_t n) const {
    return fTableData[n].tflp_dimen[d0];
  }
  void set_tflp_nslat(size_t n, int v) {
    fTableData[n].tflp_nslat = v;
  }
  int get_tflp_nslat(size_t n) const {
    return fTableData[n].tflp_nslat;
  }
  void set_tflp_isegm(size_t n, int v) {
    fTableData[n].tflp_isegm = v;
  }
  int get_tflp_isegm(size_t n) const {
    return fTableData[n].tflp_isegm;
  }
  void set_color_tof(size_t n, int v) {
    fTableData[n].color_tof = v;
  }
  int get_color_tof(size_t n) const {
    return fTableData[n].color_tof;
  }
  void set_med_tof(size_t n, int v) {
    fTableData[n].med_tof = v;
  }
  int get_med_tof(size_t n) const {
    return fTableData[n].med_tof;
  }

private:
  TOFPARA_ST* fTableData;

  ClassDef(tofparaWrapper,1)
};
#endif /*__TOFPARAWRAPPER_H__*/
