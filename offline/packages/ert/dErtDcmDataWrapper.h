#ifndef __DERTDCMDATAWRAPPER_H__
#define __DERTDCMDATAWRAPPER_H__
/* Automatically generated.  Do not edit. */
#include <stddef.h>
#include "PHTable.hh"
#include "dErtDcmData.h"
class dErtDcmDataWrapper: public PHTable
{
public:
  dErtDcmDataWrapper(const char* name = "dErtDcmData", const size_t& max_rows = 1);
  dErtDcmDataWrapper(const dErtDcmDataWrapper& source);
  dErtDcmDataWrapper& operator=(const dErtDcmDataWrapper& source);

  ~dErtDcmDataWrapper();

  void* RawTableData();
  DERTDCMDATA_ST* TableData();

  DERTDCMDATA_ST& operator[](const size_t& row);
  const DERTDCMDATA_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_Nwords(size_t n, unsigned long v) {
    fTableData[n].Nwords = v;
  }
  unsigned long get_Nwords(size_t n) const {
    return fTableData[n].Nwords;
  }
  void set_packetID(size_t n, unsigned long v) {
    fTableData[n].packetID = v;
  }
  unsigned long get_packetID(size_t n) const {
    return fTableData[n].packetID;
  }
  void set_hitformat(size_t n, unsigned long v) {
    fTableData[n].hitformat = v;
  }
  unsigned long get_hitformat(size_t n) const {
    return fTableData[n].hitformat;
  }
  void set_word(size_t d0, size_t n, unsigned long v) {
    fTableData[n].word[d0] = v;
  }
  unsigned long get_word(size_t d0, size_t n) const {
    return fTableData[n].word[d0];
  }

private:
  DERTDCMDATA_ST* fTableData;

  ClassDef(dErtDcmDataWrapper,1)
};
#endif /*__DERTDCMDATAWRAPPER_H__*/
