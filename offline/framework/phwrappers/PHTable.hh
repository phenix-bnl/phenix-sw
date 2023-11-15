#ifndef __STAF_TABLE__
#define __STAF_TABLE__

#include <stddef.h>
#include "TObject.h"
#include "TBuffer.h"
#include "TMemberInspector.h"
#include "table_header.h"

// typedef struct  table_head_st {
//    char name[20];       /* table name */
//    char type[20];       /* table type */
//    long maxlen;         /* # rows allocated */
//    long nok;            /* # rows filled */
//    long rbytes;         /* number of bytes per row */
//    long dsl_pointer;    /* swizzled (DS_DATASET_T*) */
//    long data_pointer;   /* swizzled (char*) */
// } TABLE_HEAD_ST;

class PHTable : public TObject
{
public:
  PHTable(const char* name = "", const size_t& max_rows = 1);

  virtual ~PHTable();

  virtual void* RawTableData();
  TABLE_HEAD_ST TableHeader() const;
  size_t MaxRowCount() const;
  size_t RowCount() const;
  size_t RowSize() const;
  bool IsEmpty() const;
  void Show() const;

  virtual void SetMaxRowCount(const size_t& max_rows) = 0;
  virtual void SetRowCount(const size_t& n) = 0;
  virtual void SetRowSize(const size_t& row_size) = 0;
  virtual void SetType(const char* type);

protected:
  TABLE_HEAD_ST* fTableHeader;

  ClassDef(PHTable,1)
};

#endif // __STAF_TABLE__
