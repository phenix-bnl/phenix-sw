#ifndef __DTOFDCMWRAPPER_H__
#define __DTOFDCMWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofDCM.h"
class dTofDCMWrapper: public PHTable
{
public:
  dTofDCMWrapper(const char* name = "dTofDCM", const size_t& max_rows = 1);
  dTofDCMWrapper(const dTofDCMWrapper& source);
  dTofDCMWrapper& operator=(const dTofDCMWrapper& source);

  ~dTofDCMWrapper();

  void* RawTableData();
  DTOFDCM_ST* TableData();

  DTOFDCM_ST& operator[](const size_t& row);
  const DTOFDCM_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

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
  DTOFDCM_ST* fTableData;

  ClassDef(dTofDCMWrapper,1)
};
#endif /*__DTOFDCMWRAPPER_H__*/
