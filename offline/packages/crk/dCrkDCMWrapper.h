#ifndef __DCRKDCMWRAPPER_H__
#define __DCRKDCMWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkDCM.h"
class dCrkDCMWrapper: public PHTable
{
public:
  dCrkDCMWrapper(const char* name = "dCrkDCM", const size_t& max_rows = 1);
//  dCrkDCMWrapper(const dCrkDCMWrapper& source);
//  dCrkDCMWrapper& operator=(const dCrkDCMWrapper& source);

  ~dCrkDCMWrapper();

  void* RawTableData();
  DCRKDCM_ST* TableData();

  DCRKDCM_ST& operator[](const size_t& row);
  const DCRKDCM_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_nWord(size_t n, unsigned int v) {
    fTableData[n].nWord = v;
  }
  unsigned int get_nWord(size_t n) const {
    return fTableData[n].nWord;
  }
  void set_scheme(size_t n, unsigned int v) {
    fTableData[n].scheme = v;
  }
  unsigned int get_scheme(size_t n) const {
    return fTableData[n].scheme;
  }
  void set_packetID(size_t n, unsigned int v) {
    fTableData[n].packetID = v;
  }
  unsigned int get_packetID(size_t n) const {
    return fTableData[n].packetID;
  }
  void set_flag(size_t n, unsigned int v) {
    fTableData[n].flag = v;
  }
  unsigned int get_flag(size_t n) const {
    return fTableData[n].flag;
  }
  void set_module(size_t n, unsigned int v) {
    fTableData[n].module = v;
  }
  unsigned int get_module(size_t n) const {
    return fTableData[n].module;
  }
  void set_evno(size_t n, unsigned int v) {
    fTableData[n].evno = v;
  }
  unsigned int get_evno(size_t n) const {
    return fTableData[n].evno;
  }
  void set_clock(size_t n, unsigned int v) {
    fTableData[n].clock = v;
  }
  unsigned int get_clock(size_t n) const {
    return fTableData[n].clock;
  }
  void set_detid(size_t n, unsigned int v) {
    fTableData[n].detid = v;
  }
  unsigned int get_detid(size_t n) const {
    return fTableData[n].detid;
  }
  void set_tac_cell(size_t n, unsigned int v) {
    fTableData[n].tac_cell = v;
  }
  unsigned int get_tac_cell(size_t n) const {
    return fTableData[n].tac_cell;
  }
  void set_pre_cell(size_t n, unsigned int v) {
    fTableData[n].pre_cell = v;
  }
  unsigned int get_pre_cell(size_t n) const {
    return fTableData[n].pre_cell;
  }
  void set_post_cell(size_t n, unsigned int v) {
    fTableData[n].post_cell = v;
  }
  unsigned int get_post_cell(size_t n) const {
    return fTableData[n].post_cell;
  }
  void set_data(size_t d0, size_t n, unsigned int v) {
    fTableData[n].data[d0] = v;
  }
  unsigned int get_data(size_t d0, size_t n) const {
    return fTableData[n].data[d0];
  }

private:
  DCRKDCM_ST* fTableData;

  ClassDef(dCrkDCMWrapper,1)
};
#endif /*__DCRKDCMWRAPPER_H__*/
