#ifndef __DEMCRECOPARWRAPPER_H__
#define __DEMCRECOPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcRecoPar.h"
class dEmcRecoParWrapper: public PHTable
{
public:
  dEmcRecoParWrapper(const char* name = "dEmcRecoPar", const size_t& max_rows = 1);
//  dEmcRecoParWrapper(const dEmcRecoParWrapper& source);
//  dEmcRecoParWrapper& operator=(const dEmcRecoParWrapper& source);

  ~dEmcRecoParWrapper();

  void* RawTableData();
  DEMCRECOPAR_ST* TableData();

  DEMCRECOPAR_ST& operator[](const size_t& row);
  const DEMCRECOPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_anyset(size_t n, int v) {
    fTableData[n].anyset = v;
  }
  int get_anyset(size_t n) const {
    return fTableData[n].anyset;
  }
  void set_extended_output(size_t n, int v) {
    fTableData[n].extended_output = v;
  }
  int get_extended_output(size_t n) const {
    return fTableData[n].extended_output;
  }
  void set_maxtowers(size_t n, int v) {
    fTableData[n].maxtowers = v;
  }
  int get_maxtowers(size_t n) const {
    return fTableData[n].maxtowers;
  }
  void set_clus_thr(size_t n, float v) {
    fTableData[n].clus_thr = v;
  }
  float get_clus_thr(size_t n) const {
    return fTableData[n].clus_thr;
  }
  void set_clus_cut(size_t n, float v) {
    fTableData[n].clus_cut = v;
  }
  float get_clus_cut(size_t n) const {
    return fTableData[n].clus_cut;
  }
  void set_clus_w0(size_t n, float v) {
    fTableData[n].clus_w0 = v;
  }
  float get_clus_w0(size_t n) const {
    return fTableData[n].clus_w0;
  }
  void set_TowerThresh(size_t n, float v) {
    fTableData[n].TowerThresh = v;
  }
  float get_TowerThresh(size_t n) const {
    return fTableData[n].TowerThresh;
  }
  void set_eClustMin(size_t n, float v) {
    fTableData[n].eClustMin = v;
  }
  float get_eClustMin(size_t n) const {
    return fTableData[n].eClustMin;
  }
  void set_tmppar(size_t n, float v) {
    fTableData[n].tmppar = v;
  }
  float get_tmppar(size_t n) const {
    return fTableData[n].tmppar;
  }

private:
  DEMCRECOPAR_ST* fTableData;

  ClassDef(dEmcRecoParWrapper,1)
};
#endif /*__DEMCRECOPARWRAPPER_H__*/
