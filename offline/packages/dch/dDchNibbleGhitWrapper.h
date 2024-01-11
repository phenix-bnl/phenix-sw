#ifndef __DDCHNIBBLEGHITWRAPPER_H__
#define __DDCHNIBBLEGHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchNibbleGhit.h"
class dDchNibbleGhitWrapper: public PHTable
{
public:
  dDchNibbleGhitWrapper(const char* name = "dDchNibbleGhit", const size_t& max_rows = 1);
  ~dDchNibbleGhitWrapper();

  void* RawTableData();
  DDCHNIBBLEGHIT_ST* TableData();

  DDCHNIBBLEGHIT_ST& operator[](const size_t& row);
  const DDCHNIBBLEGHIT_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_ghitid(size_t n, short v) {
    fTableData[n].ghitid = v;
  }
  short get_ghitid(size_t n) const {
    return fTableData[n].ghitid;
  }
  void set_arm(size_t n, short v) {
    fTableData[n].arm = v;
  }
  short get_arm(size_t n) const {
    return fTableData[n].arm;
  }
  void set_side(size_t n, short v) {
    fTableData[n].side = v;
  }
  short get_side(size_t n) const {
    return fTableData[n].side;
  }
  void set_key(size_t n, short v) {
    fTableData[n].key = v;
  }
  short get_key(size_t n) const {
    return fTableData[n].key;
  }
  void set_pair(size_t n, short v) {
    fTableData[n].pair = v;
  }
  short get_pair(size_t n) const {
    return fTableData[n].pair;
  }
  void set_channel(size_t n, short v) {
    fTableData[n].channel = v;
  }
  short get_channel(size_t n) const {
    return fTableData[n].channel;
  }
  void set_Nibble(size_t n, short v) {
    fTableData[n].Nibble = v;
  }
  short get_Nibble(size_t n) const {
    return fTableData[n].Nibble;
  }

private:
  DDCHNIBBLEGHIT_ST* fTableData;

  ClassDef(dDchNibbleGhitWrapper,1)
};
#endif /*__DDCHNIBBLEGHITWRAPPER_H__*/
