#ifndef __DBBCEVALWRAPPER_H__
#define __DBBCEVALWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dBbcEval.h"
class dBbcEvalWrapper: public PHTable
{
public:
  dBbcEvalWrapper(const char* name = "dBbcEval", const size_t& max_rows = 1);
  dBbcEvalWrapper(const dBbcEvalWrapper& source);
  dBbcEvalWrapper& operator=(const dBbcEvalWrapper& source);

  ~dBbcEvalWrapper();

  void* RawTableData();
  DBBCEVAL_ST* TableData();

  DBBCEVAL_ST& operator[](const size_t& row);
  const DBBCEVAL_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_NHitPmtSouth(size_t n, short v) {
    fTableData[n].NHitPmtSouth = v;
  }
  short get_NHitPmtSouth(size_t n) const {
    return fTableData[n].NHitPmtSouth;
  }
  void set_NHitPmtNorth(size_t n, short v) {
    fTableData[n].NHitPmtNorth = v;
  }
  short get_NHitPmtNorth(size_t n) const {
    return fTableData[n].NHitPmtNorth;
  }
  void set_ChargeSumNorth(size_t n, float v) {
    fTableData[n].ChargeSumNorth = v;
  }
  float get_ChargeSumNorth(size_t n) const {
    return fTableData[n].ChargeSumNorth;
  }
  void set_ChargeSumSouth(size_t n, float v) {
    fTableData[n].ChargeSumSouth = v;
  }
  float get_ChargeSumSouth(size_t n) const {
    return fTableData[n].ChargeSumSouth;
  }
  void set_b(size_t n, float v) {
    fTableData[n].b = v;
  }
  float get_b(size_t n) const {
    return fTableData[n].b;
  }
  void set_TimeZero(size_t n, float v) {
    fTableData[n].TimeZero = v;
  }
  float get_TimeZero(size_t n) const {
    return fTableData[n].TimeZero;
  }
  void set_ZVertexCal(size_t n, float v) {
    fTableData[n].ZVertexCal = v;
  }
  float get_ZVertexCal(size_t n) const {
    return fTableData[n].ZVertexCal;
  }
  void set_ZVertexSim(size_t n, float v) {
    fTableData[n].ZVertexSim = v;
  }
  float get_ZVertexSim(size_t n) const {
    return fTableData[n].ZVertexSim;
  }

private:
  DBBCEVAL_ST* fTableData;

  ClassDef(dBbcEvalWrapper,1)
};
#endif /*__DBBCEVALWRAPPER_H__*/
