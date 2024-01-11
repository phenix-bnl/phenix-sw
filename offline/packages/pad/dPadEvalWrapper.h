#ifndef __DPADEVALWRAPPER_H__
#define __DPADEVALWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadEval.h"
class dPadEvalWrapper: public PHTable
{
public:
  dPadEvalWrapper(const char* name = "dPadEval", const size_t& max_rows = 1);
//  dPadEvalWrapper(const dPadEvalWrapper& source);
//  dPadEvalWrapper& operator=(const dPadEvalWrapper& source);

  ~dPadEvalWrapper();

  void* RawTableData();
  DPADEVAL_ST* TableData();

  DPADEVAL_ST& operator[](const size_t& row);
  const DPADEVAL_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_clusid(size_t n, short v) {
    fTableData[n].clusid = v;
  }
  short get_clusid(size_t n) const {
    return fTableData[n].clusid;
  }
  void set_ghitid(size_t n, short v) {
    fTableData[n].ghitid = v;
  }
  short get_ghitid(size_t n) const {
    return fTableData[n].ghitid;
  }
  void set_deltax(size_t n, float v) {
    fTableData[n].deltax = v;
  }
  float get_deltax(size_t n) const {
    return fTableData[n].deltax;
  }
  void set_deltaz(size_t n, float v) {
    fTableData[n].deltaz = v;
  }
  float get_deltaz(size_t n) const {
    return fTableData[n].deltaz;
  }
  void set_deltar(size_t n, float v) {
    fTableData[n].deltar = v;
  }
  float get_deltar(size_t n) const {
    return fTableData[n].deltar;
  }
  void set_nhits(size_t n, short v) {
    fTableData[n].nhits = v;
  }
  short get_nhits(size_t n) const {
    return fTableData[n].nhits;
  }
  void set_type(size_t n, short v) {
    fTableData[n].type = v;
  }
  short get_type(size_t n) const {
    return fTableData[n].type;
  }

private:
  DPADEVAL_ST* fTableData;

  ClassDef(dPadEvalWrapper,1)
};
#endif /*__DPADEVALWRAPPER_H__*/
