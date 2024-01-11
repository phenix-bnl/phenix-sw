#ifndef __DTOFEVTHEADERWRAPPER_H__
#define __DTOFEVTHEADERWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofEvtHeader.h"
class dTofEvtHeaderWrapper: public PHTable
{
public:
  dTofEvtHeaderWrapper(const char* name = "dTofEvtHeader", const size_t& max_rows = 1);
  dTofEvtHeaderWrapper(const dTofEvtHeaderWrapper& source);
  dTofEvtHeaderWrapper& operator=(const dTofEvtHeaderWrapper& source);

  ~dTofEvtHeaderWrapper();

  void* RawTableData();
  DTOFEVTHEADER_ST* TableData();

  DTOFEVTHEADER_ST& operator[](const size_t& row);
  const DTOFEVTHEADER_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option);

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_run(size_t n, int v) {
    fTableData[n].run = v;
  }
  int get_run(size_t n) const {
    return fTableData[n].run;
  }
  void set_date(size_t n, int v) {
    fTableData[n].date = v;
  }
  int get_date(size_t n) const {
    return fTableData[n].date;
  }
  void set_time(size_t n, int v) {
    fTableData[n].time = v;
  }
  int get_time(size_t n) const {
    return fTableData[n].time;
  }
  void set_evtseq(size_t n, int v) {
    fTableData[n].evtseq = v;
  }
  int get_evtseq(size_t n) const {
    return fTableData[n].evtseq;
  }
  void set_scaledtrig(size_t n, int v) {
    fTableData[n].scaledtrig = v;
  }
  int get_scaledtrig(size_t n) const {
    return fTableData[n].scaledtrig;
  }
  void set_rawtrig(size_t n, int v) {
    fTableData[n].rawtrig = v;
  }
  int get_rawtrig(size_t n) const {
    return fTableData[n].rawtrig;
  }
  void set_livetrig(size_t n, int v) {
    fTableData[n].livetrig = v;
  }
  int get_livetrig(size_t n) const {
    return fTableData[n].livetrig;
  }

private:
  DTOFEVTHEADER_ST* fTableData;

  ClassDef(dTofEvtHeaderWrapper,1)
};
#endif /*__DTOFEVTHEADERWRAPPER_H__*/
