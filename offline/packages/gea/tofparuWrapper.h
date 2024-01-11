#ifndef __TOFPARUWRAPPER_H__
#define __TOFPARUWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "tofparu.h"
class tofparuWrapper: public PHTable
{
public:
  tofparuWrapper(const char* name = "tofparu", const size_t& max_rows = 1);
//  tofparuWrapper(const tofparuWrapper& source);
//  tofparuWrapper& operator=(const tofparuWrapper& source);

  ~tofparuWrapper();

  void* RawTableData();
  TOFPARU_ST* TableData();

  TOFPARU_ST& operator[](const size_t& row);
  const TOFPARU_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_tfss_dimen(size_t d0, size_t n, float v) {
    fTableData[n].tfss_dimen[d0] = v;
  }
  float get_tfss_dimen(size_t d0, size_t n) const {
    return fTableData[n].tfss_dimen[d0];
  }
  void set_tfss_length(size_t n, float v) {
    fTableData[n].tfss_length = v;
  }
  float get_tfss_length(size_t n) const {
    return fTableData[n].tfss_length;
  }
  void set_mff_haddets(size_t n, int v) {
    fTableData[n].mff_haddets = v;
  }
  int get_mff_haddets(size_t n) const {
    return fTableData[n].mff_haddets;
  }
  void set_tfls_dimen(size_t d0, size_t n, float v) {
    fTableData[n].tfls_dimen[d0] = v;
  }
  float get_tfls_dimen(size_t d0, size_t n) const {
    return fTableData[n].tfls_dimen[d0];
  }
  void set_tfls_length(size_t n, float v) {
    fTableData[n].tfls_length = v;
  }
  float get_tfls_length(size_t n) const {
    return fTableData[n].tfls_length;
  }
  void set_mff_eledets(size_t n, int v) {
    fTableData[n].mff_eledets = v;
  }
  int get_mff_eledets(size_t n) const {
    return fTableData[n].mff_eledets;
  }
  void set_mff_alldets(size_t n, int v) {
    fTableData[n].mff_alldets = v;
  }
  int get_mff_alldets(size_t n) const {
    return fTableData[n].mff_alldets;
  }

private:
  TOFPARU_ST* fTableData;

  ClassDef(tofparuWrapper,1)
};
#endif /*__TOFPARUWRAPPER_H__*/
