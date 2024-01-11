#ifndef __PYTHIAWRAPPER_H__
#define __PYTHIAWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "pythia.h"
class pythiaWrapper: public PHTable
{
public:
  pythiaWrapper(const char* name = "pythia", const size_t& max_rows = 1);
//  pythiaWrapper(const pythiaWrapper& source);
//  pythiaWrapper& operator=(const pythiaWrapper& source);

  ~pythiaWrapper();

  void* RawTableData();
  PYTHIA_ST* TableData();

  PYTHIA_ST& operator[](const size_t& row);
  const PYTHIA_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_pyth_proc_id(size_t n, short v) {
    fTableData[n].pyth_proc_id = v;
  }
  short get_pyth_proc_id(size_t n) const {
    return fTableData[n].pyth_proc_id;
  }
  void set_pyth_bjork(size_t d0, size_t n, float v) {
    fTableData[n].pyth_bjork[d0] = v;
  }
  float get_pyth_bjork(size_t d0, size_t n) const {
    return fTableData[n].pyth_bjork[d0];
  }
  void set_pyth_parstu(size_t d0, size_t n, float v) {
    fTableData[n].pyth_parstu[d0] = v;
  }
  float get_pyth_parstu(size_t d0, size_t n) const {
    return fTableData[n].pyth_parstu[d0];
  }
  void set_pyth_qsqr(size_t n, float v) {
    fTableData[n].pyth_qsqr = v;
  }
  float get_pyth_qsqr(size_t n) const {
    return fTableData[n].pyth_qsqr;
  }
  void set_pyth_ptrans(size_t n, float v) {
    fTableData[n].pyth_ptrans = v;
  }
  float get_pyth_ptrans(size_t n) const {
    return fTableData[n].pyth_ptrans;
  }
  void set_intr_part_id(size_t d0, size_t n, short v) {
    fTableData[n].intr_part_id[d0] = v;
  }
  short get_intr_part_id(size_t d0, size_t n) const {
    return fTableData[n].intr_part_id[d0];
  }
  void set_intr_part_p(size_t d0, size_t d1, size_t n, float v) {
    fTableData[n].intr_part_p[d0][d1] = v;
  }
  float get_intr_part_p(size_t d0, size_t d1, size_t n) const {
    return fTableData[n].intr_part_p[d0][d1];
  }

private:
  PYTHIA_ST* fTableData; //! Tell roocint this is "transient" (suppresses warnings about streamers)

  ClassDef(pythiaWrapper,1)
};
#endif /*__PYTHIAWRAPPER_H__*/
