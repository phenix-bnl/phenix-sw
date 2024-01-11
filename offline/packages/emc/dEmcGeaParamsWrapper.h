#ifndef __DEMCGEAPARAMSWRAPPER_H__
#define __DEMCGEAPARAMSWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcGeaParams.h"
class dEmcGeaParamsWrapper: public PHTable
{
public:
  dEmcGeaParamsWrapper(const char* name = "dEmcGeaParams", const size_t& max_rows = 1);
//  dEmcGeaParamsWrapper(const dEmcGeaParamsWrapper& source);
//  dEmcGeaParamsWrapper& operator=(const dEmcGeaParamsWrapper& source);

  ~dEmcGeaParamsWrapper();

  void* RawTableData();
  DEMCGEAPARAMS_ST* TableData();

  DEMCGEAPARAMS_ST& operator[](const size_t& row);
  const DEMCGEAPARAMS_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_detarray(size_t d0, size_t n, float v) {
    fTableData[n].detarray[d0] = v;
  }
  float get_detarray(size_t d0, size_t n) const {
    return fTableData[n].detarray[d0];
  }

private:
  DEMCGEAPARAMS_ST* fTableData;

  ClassDef(dEmcGeaParamsWrapper,1)
};
#endif /*__DEMCGEAPARAMSWRAPPER_H__*/
