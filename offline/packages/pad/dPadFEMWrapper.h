#ifndef __DPADFEMWRAPPER_H__
#define __DPADFEMWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadFEM.h"
class dPadFEMWrapper: public PHTable
{
public:
  dPadFEMWrapper(const char* name = "dPadFEM", const size_t& max_rows = 1);
//  dPadFEMWrapper(const dPadFEMWrapper& source);
//  dPadFEMWrapper& operator=(const dPadFEMWrapper& source);

  ~dPadFEMWrapper();

  void* RawTableData();
  DPADFEM_ST* TableData();

  DPADFEM_ST& operator[](const size_t& row);
  const DPADFEM_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_CAV1(size_t n, int v) {
    fTableData[n].CAV1 = v;
  }
  int get_CAV1(size_t n) const {
    return fTableData[n].CAV1;
  }
  void set_det(size_t n, int v) {
    fTableData[n].det = v;
  }
  int get_det(size_t n) const {
    return fTableData[n].det;
  }
  void set_Ecounter(size_t n, int v) {
    fTableData[n].Ecounter = v;
  }
  int get_Ecounter(size_t n) const {
    return fTableData[n].Ecounter;
  }
  void set_adr(size_t n, int v) {
    fTableData[n].adr = v;
  }
  int get_adr(size_t n) const {
    return fTableData[n].adr;
  }
  void set_Flag(size_t n, int v) {
    fTableData[n].Flag = v;
  }
  int get_Flag(size_t n) const {
    return fTableData[n].Flag;
  }
  void set_Bcounter(size_t n, int v) {
    fTableData[n].Bcounter = v;
  }
  int get_Bcounter(size_t n) const {
    return fTableData[n].Bcounter;
  }
  void set_Word(size_t d0, size_t n, int v) {
    fTableData[n].Word[d0] = v;
  }
  int get_Word(size_t d0, size_t n) const {
    return fTableData[n].Word[d0];
  }
  void set_usr1(size_t n, int v) {
    fTableData[n].usr1 = v;
  }
  int get_usr1(size_t n) const {
    return fTableData[n].usr1;
  }
  void set_usr2(size_t n, int v) {
    fTableData[n].usr2 = v;
  }
  int get_usr2(size_t n) const {
    return fTableData[n].usr2;
  }
  void set_usr3(size_t n, int v) {
    fTableData[n].usr3 = v;
  }
  int get_usr3(size_t n) const {
    return fTableData[n].usr3;
  }
  void set_usr4(size_t n, int v) {
    fTableData[n].usr4 = v;
  }
  int get_usr4(size_t n) const {
    return fTableData[n].usr4;
  }
  void set_usr5(size_t n, int v) {
    fTableData[n].usr5 = v;
  }
  int get_usr5(size_t n) const {
    return fTableData[n].usr5;
  }
  void set_usr6(size_t n, int v) {
    fTableData[n].usr6 = v;
  }
  int get_usr6(size_t n) const {
    return fTableData[n].usr6;
  }
  void set_usr7(size_t n, int v) {
    fTableData[n].usr7 = v;
  }
  int get_usr7(size_t n) const {
    return fTableData[n].usr7;
  }
  void set_usr8(size_t n, int v) {
    fTableData[n].usr8 = v;
  }
  int get_usr8(size_t n) const {
    return fTableData[n].usr8;
  }
  void set_parity(size_t n, int v) {
    fTableData[n].parity = v;
  }
  int get_parity(size_t n) const {
    return fTableData[n].parity;
  }
  void set_CAV2(size_t n, int v) {
    fTableData[n].CAV2 = v;
  }
  int get_CAV2(size_t n) const {
    return fTableData[n].CAV2;
  }

private:
  DPADFEM_ST* fTableData;

  ClassDef(dPadFEMWrapper,1)
};
#endif /*__DPADFEMWRAPPER_H__*/
