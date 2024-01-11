#include <cstdlib>
#include "headerWrapper.h"

using namespace std;

//ClassImp(headerWrapper)

headerWrapper::headerWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(HEADER_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new HEADER_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new HEADER_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("header");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

headerWrapper::~headerWrapper()
{
  delete [] fTableData;
}

void*
headerWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

HEADER_ST*
headerWrapper::TableData()
{
  return fTableData;
}

HEADER_ST&
headerWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const HEADER_ST&
headerWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
headerWrapper::SetMaxRowCount(const size_t& max_rows)
{
  // Avoid reallocing a space of zero size!
  if (max_rows <= 0) {
     return;
  }

  // Ensure that the current row count is not out of range.
  if (fTableHeader->nok > max_rows) {
     fTableHeader->nok = max_rows;
  }

  // If table needs to grow, allocate a new area for it.
  if (max_rows > fTableHeader->maxlen) {
     HEADER_ST* newData = new HEADER_ST[max_rows];
     if (fTableData) {
        for (long i = 0; i < fTableHeader->nok; i++) {
           newData[i] = fTableData[i];
        }
        delete [] fTableData;
     }
     fTableData = newData;
     fTableHeader->data_pointer = (long)fTableData;
  }

  fTableHeader->maxlen = max_rows;
}
void
headerWrapper::SetRowCount(const size_t& n)
{
  if (n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
headerWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

class bbcghitWrapper
{
public:
  bbcghitWrapper(char const *a, unsigned int const &b);
  virtual ~bbcghitWrapper() {}
  void TableData();
};

bbcghitWrapper::bbcghitWrapper(char const *a, unsigned int const &b)
{
}

void bbcghitWrapper::TableData()
{
}

void ds2ReallocTable(TABLE_HEAD_ST **a, char **b, unsigned int c)
{
}
