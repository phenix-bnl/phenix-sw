#ifndef __DDCHDCMWRAPPER_H__
#define __DDCHDCMWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchDCM.h"
class dDchDCMWrapper: public PHTable
{
public:
  dDchDCMWrapper(const char* name = "dDchDCM", const size_t& max_rows = 1);
  ~dDchDCMWrapper();

  void* RawTableData();
  DDCHDCM_ST* TableData();

  DDCHDCM_ST& operator[](const size_t& row);
  const DDCHDCM_ST& operator[](const size_t& row) const;

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
  void set_DCM(size_t d0, size_t n, unsigned int v) {
    fTableData[n].DCM[d0] = v;
  }
  unsigned int get_DCM(size_t d0, size_t n) const {
    return fTableData[n].DCM[d0];
  }

private:
  DDCHDCM_ST* fTableData;

  ClassDef(dDchDCMWrapper,1)
};
#endif /*__DDCHDCMWRAPPER_H__*/
