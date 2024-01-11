#ifndef __DBBCOUTWRAPPER_H__
#define __DBBCOUTWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dBbcOut.h"
class dBbcOutWrapper: public PHTable
{
public:
  dBbcOutWrapper(const char* name = "dBbcOut", const size_t& max_rows = 1);
  dBbcOutWrapper(const dBbcOutWrapper& source);
  dBbcOutWrapper& operator=(const dBbcOutWrapper& source);

  ~dBbcOutWrapper();

  void* RawTableData();
  DBBCOUT_ST* TableData();

  DBBCOUT_ST& operator[](const size_t& row);
  const DBBCOUT_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_NhitPmtNorth(size_t n, short v) {
    fTableData[n].NhitPmtNorth = v;
  }
  short get_NhitPmtNorth(size_t n) const {
    return fTableData[n].NhitPmtNorth;
  }
  void set_NhitPmtSouth(size_t n, short v) {
    fTableData[n].NhitPmtSouth = v;
  }
  short get_NhitPmtSouth(size_t n) const {
    return fTableData[n].NhitPmtSouth;
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
  void set_VertexPoint(size_t n, float v) {
    fTableData[n].VertexPoint = v;
  }
  float get_VertexPoint(size_t n) const {
    return fTableData[n].VertexPoint;
  }
  void set_dVertexPoint(size_t n, float v) {
    fTableData[n].dVertexPoint = v;
  }
  float get_dVertexPoint(size_t n) const {
    return fTableData[n].dVertexPoint;
  }
  void set_TimeZero(size_t n, float v) {
    fTableData[n].TimeZero = v;
  }
  float get_TimeZero(size_t n) const {
    return fTableData[n].TimeZero;
  }
  void set_dTimeZero(size_t n, float v) {
    fTableData[n].dTimeZero = v;
  }
  float get_dTimeZero(size_t n) const {
    return fTableData[n].dTimeZero;
  }
  void set_Adc(size_t d0, size_t n, short v) {
    fTableData[n].Adc[d0] = v;
  }
  short get_Adc(size_t d0, size_t n) const {
    return fTableData[n].Adc[d0];
  }
  void set_Tdc(size_t d0, size_t n, short v) {
    fTableData[n].Tdc[d0] = v;
  }
  short get_Tdc(size_t d0, size_t n) const {
    return fTableData[n].Tdc[d0];
  }

private:
  DBBCOUT_ST* fTableData;

  ClassDef(dBbcOutWrapper,1)
};
#endif /*__DBBCOUTWRAPPER_H__*/
