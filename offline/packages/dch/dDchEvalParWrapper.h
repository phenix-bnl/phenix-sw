#ifndef __DDCHEVALPARWRAPPER_H__
#define __DDCHEVALPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchEvalPar.h"
class dDchEvalParWrapper: public PHTable
{
public:
  dDchEvalParWrapper(const char* name = "dDchEvalPar", const size_t& max_rows = 1);
  ~dDchEvalParWrapper();

  void* RawTableData();
  DDCHEVALPAR_ST* TableData();

  DDCHEVALPAR_ST& operator[](const size_t& row);
  const DDCHEVALPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_verbose(size_t n, short v) {
    fTableData[n].verbose = v;
  }
  short get_verbose(size_t n) const {
    return fTableData[n].verbose;
  }
  void set_coordinates(size_t n, short v) {
    fTableData[n].coordinates = v;
  }
  short get_coordinates(size_t n) const {
    return fTableData[n].coordinates;
  }
  void set_main(size_t n, short v) {
    fTableData[n].main = v;
  }
  short get_main(size_t n) const {
    return fTableData[n].main;
  }
  void set_effiMainCut(size_t n, short v) {
    fTableData[n].effiMainCut = v;
  }
  short get_effiMainCut(size_t n) const {
    return fTableData[n].effiMainCut;
  }
  void set_effiDphiCut(size_t n, float v) {
    fTableData[n].effiDphiCut = v;
  }
  float get_effiDphiCut(size_t n) const {
    return fTableData[n].effiDphiCut;
  }
  void set_effiDalphaCut(size_t n, float v) {
    fTableData[n].effiDalphaCut = v;
  }
  float get_effiDalphaCut(size_t n) const {
    return fTableData[n].effiDalphaCut;
  }
  void set_effiDzedCut(size_t n, float v) {
    fTableData[n].effiDzedCut = v;
  }
  float get_effiDzedCut(size_t n) const {
    return fTableData[n].effiDzedCut;
  }
  void set_ghostMainCut(size_t n, short v) {
    fTableData[n].ghostMainCut = v;
  }
  short get_ghostMainCut(size_t n) const {
    return fTableData[n].ghostMainCut;
  }
  void set_ghostDphiCut(size_t n, float v) {
    fTableData[n].ghostDphiCut = v;
  }
  float get_ghostDphiCut(size_t n) const {
    return fTableData[n].ghostDphiCut;
  }
  void set_ghostDalphaCut(size_t n, float v) {
    fTableData[n].ghostDalphaCut = v;
  }
  float get_ghostDalphaCut(size_t n) const {
    return fTableData[n].ghostDalphaCut;
  }
  void set_ghostDzedCut(size_t n, float v) {
    fTableData[n].ghostDzedCut = v;
  }
  float get_ghostDzedCut(size_t n) const {
    return fTableData[n].ghostDzedCut;
  }

private:
  DDCHEVALPAR_ST* fTableData;

  ClassDef(dDchEvalParWrapper,1)
};
#endif /*__DDCHEVALPARWRAPPER_H__*/
