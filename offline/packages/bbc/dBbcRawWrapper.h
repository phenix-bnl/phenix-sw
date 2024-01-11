#ifndef __DBBCRAWWRAPPER_H__
#define __DBBCRAWWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dBbcRaw.h"
class dBbcRawWrapper: public PHTable
{
public:
  dBbcRawWrapper(const char* name = "dBbcRaw", const size_t& max_rows = 1);
  dBbcRawWrapper(const dBbcRawWrapper& source);
  dBbcRawWrapper& operator=(const dBbcRawWrapper& source);

  ~dBbcRawWrapper();

  void* RawTableData();
  DBBCRAW_ST* TableData();

  DBBCRAW_ST& operator[](const size_t& row);
  const DBBCRAW_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_Pmt(size_t n, short v) {
    fTableData[n].Pmt = v;
  }
  short get_Pmt(size_t n) const {
    return fTableData[n].Pmt;
  }
  void set_Arm(size_t n, short v) {
    fTableData[n].Arm = v;
  }
  short get_Arm(size_t n) const {
    return fTableData[n].Arm;
  }
  void set_Half(size_t n, short v) {
    fTableData[n].Half = v;
  }
  short get_Half(size_t n) const {
    return fTableData[n].Half;
  }
  void set_Ring(size_t n, short v) {
    fTableData[n].Ring = v;
  }
  short get_Ring(size_t n) const {
    return fTableData[n].Ring;
  }
  void set_Tube(size_t n, short v) {
    fTableData[n].Tube = v;
  }
  short get_Tube(size_t n) const {
    return fTableData[n].Tube;
  }
  void set_Adc(size_t n, short v) {
    fTableData[n].Adc = v;
  }
  short get_Adc(size_t n) const {
    return fTableData[n].Adc;
  }
  void set_Tdc0(size_t n, short v) {
    fTableData[n].Tdc0 = v;
  }
  short get_Tdc0(size_t n) const {
    return fTableData[n].Tdc0;
  }
  void set_Tdc1(size_t n, short v) {
    fTableData[n].Tdc1 = v;
  }
  short get_Tdc1(size_t n) const {
    return fTableData[n].Tdc1;
  }

private:
  DBBCRAW_ST* fTableData;

  ClassDef(dBbcRawWrapper,1)
};
#endif /*__DBBCRAWWRAPPER_H__*/
