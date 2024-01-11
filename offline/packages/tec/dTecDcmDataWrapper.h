#ifndef __DTECDCMDATAWRAPPER_H__
#define __DTECDCMDATAWRAPPER_H__

#include <cstddef>
#include "PHTable.hh"
#include "dTecDcmData.h"

class dTecDcmDataWrapper: public PHTable
{
public:
  dTecDcmDataWrapper(const char* name = "dTecDcmData", const size_t& max_rows = 1);
//  dTecDcmDataWrapper(const dTecDcmDataWrapper& source);
//  dTecDcmDataWrapper& operator=(const dTecDcmDataWrapper& source);

  ~dTecDcmDataWrapper();

  void* RawTableData();
  DTECDCMDATA_ST* TableData();

  DTECDCMDATA_ST& operator[](const size_t& row);
  const DTECDCMDATA_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_Nwords(size_t n, unsigned int v) {
    fTableData[n].Nwords = v;
  }
  unsigned int get_Nwords(size_t n) const {
    return fTableData[n].Nwords;
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
  DTECDCMDATA_ST* fTableData;

  ClassDef(dTecDcmDataWrapper,1)
};
#endif /*__DTECDCMDATAWRAPPER_H__*/
