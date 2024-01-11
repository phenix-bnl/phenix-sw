#ifndef __DBBCFEMWRAPPER_H__
#define __DBBCFEMWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dBbcFEM.h"
class dBbcFEMWrapper: public PHTable
{
public:
  dBbcFEMWrapper(const char* name = "dBbcFEM", const size_t& max_rows = 1);
  dBbcFEMWrapper(const dBbcFEMWrapper& source);
  dBbcFEMWrapper& operator=(const dBbcFEMWrapper& source);

  ~dBbcFEMWrapper();

  void* RawTableData();
  DBBCFEM_ST* TableData();

  DBBCFEM_ST& operator[](const size_t& row);
  const DBBCFEM_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_CAV1(size_t n, unsigned short v) {
    fTableData[n].CAV1 = v;
  }
  unsigned short get_CAV1(size_t n) const {
    return fTableData[n].CAV1;
  }
  void set_det(size_t n, unsigned short v) {
    fTableData[n].det = v;
  }
  unsigned short get_det(size_t n) const {
    return fTableData[n].det;
  }
  void set_Ecounter(size_t n, unsigned short v) {
    fTableData[n].Ecounter = v;
  }
  unsigned short get_Ecounter(size_t n) const {
    return fTableData[n].Ecounter;
  }
  void set_adr(size_t n, unsigned short v) {
    fTableData[n].adr = v;
  }
  unsigned short get_adr(size_t n) const {
    return fTableData[n].adr;
  }
  void set_Flag(size_t n, unsigned short v) {
    fTableData[n].Flag = v;
  }
  unsigned short get_Flag(size_t n) const {
    return fTableData[n].Flag;
  }
  void set_Bcounter(size_t n, unsigned short v) {
    fTableData[n].Bcounter = v;
  }
  unsigned short get_Bcounter(size_t n) const {
    return fTableData[n].Bcounter;
  }
  void set_Word(size_t d0, size_t n, unsigned short v) {
    fTableData[n].Word[d0] = v;
  }
  unsigned short get_Word(size_t d0, size_t n) const {
    return fTableData[n].Word[d0];
  }
  void set_parity(size_t n, unsigned short v) {
    fTableData[n].parity = v;
  }
  unsigned short get_parity(size_t n) const {
    return fTableData[n].parity;
  }
  void set_CAV2(size_t n, unsigned short v) {
    fTableData[n].CAV2 = v;
  }
  unsigned short get_CAV2(size_t n) const {
    return fTableData[n].CAV2;
  }

private:
  DBBCFEM_ST* fTableData;

  ClassDef(dBbcFEMWrapper,1)
};
#endif /*__DBBCFEMWRAPPER_H__*/
