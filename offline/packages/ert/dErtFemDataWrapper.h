#ifndef __DERTFEMDATAWRAPPER_H__
#define __DERTFEMDATAWRAPPER_H__
/* Automatically generated.  Do not edit. */
#include <stddef.h>
#include "PHTable.hh"
#include "dErtFemData.h"
class dErtFemDataWrapper: public PHTable
{
public:
  dErtFemDataWrapper(const char* name = "dErtFemData", const size_t& max_rows = 1);
  dErtFemDataWrapper(const dErtFemDataWrapper& source);
  dErtFemDataWrapper& operator=(const dErtFemDataWrapper& source);

  ~dErtFemDataWrapper();

  void* RawTableData();
  DERTFEMDATA_ST* TableData();

  DERTFEMDATA_ST& operator[](const size_t& row);
  const DERTFEMDATA_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_crate(size_t n, short v) {
    fTableData[n].crate = v;
  }
  short get_crate(size_t n) const {
    return fTableData[n].crate;
  }
  void set_Roc(size_t n, short v) {
    fTableData[n].Roc = v;
  }
  short get_Roc(size_t n) const {
    return fTableData[n].Roc;
  }
  void set_word(size_t n, short v) {
    fTableData[n].word = v;
  }
  short get_word(size_t n) const {
    return fTableData[n].word;
  }
  void set_Value(size_t n, long v) {
    fTableData[n].Value = v;
  }
  long get_Value(size_t n) const {
    return fTableData[n].Value;
  }

private:
  DERTFEMDATA_ST* fTableData;

  ClassDef(dErtFemDataWrapper,1)
};
#endif /*__DERTFEMDATAWRAPPER_H__*/
