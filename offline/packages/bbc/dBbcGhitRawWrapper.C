#include <cstring>
#include <iostream>
#include <iomanip>
#include "dBbcGhitRawWrapper.h"

using namespace std;

ClassImp(dBbcGhitRawWrapper)

dBbcGhitRawWrapper::dBbcGhitRawWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DBBCGHITRAW_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DBBCGHITRAW_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DBBCGHITRAW_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dBbcGhitRaw");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dBbcGhitRawWrapper::dBbcGhitRawWrapper(const dBbcGhitRawWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DBBCGHITRAW_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DBBCGHITRAW_ST));
  SetType("dBbcGhitRaw");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dBbcGhitRawWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dBbcGhitRawWrapper&
dBbcGhitRawWrapper::operator=(const dBbcGhitRawWrapper& source)
{
  if (this != &source) {
     // The row count will be set by the PHTable assignment operator.

     PHTable::operator=(source);
     
     // Just copy the data from the source table.
     for (size_t row = 0; row < RowCount(); row++) {
        fTableData[row] = source.fTableData[row];
     }

  }

  return *this;
}

dBbcGhitRawWrapper::~dBbcGhitRawWrapper()
{
  delete [] fTableData;
}

DBBCGHITRAW_ST*
dBbcGhitRawWrapper::TableData()
{
  return fTableData;
}

DBBCGHITRAW_ST&
dBbcGhitRawWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DBBCGHITRAW_ST&
dBbcGhitRawWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dBbcGhitRawWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "rawid";
  cout << " " << setw(11) << "ghitid";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].rawid;
     cout << " " << setw(11) << fTableData[row].ghitid;

     cout << endl;
  }

}

void
dBbcGhitRawWrapper::Print(Option_t* option) const
{
   // This version of Print overrides the one in the TObject
   // base class, and provides a way to call Print with no
   // arguments.  If Print(const size_t, const size_t) const
   // could be called with no arguments, there would be an
   // ambiguity.  I hope that this explanation makes sense!

   if (!option || (strlen(option) <= 0) ) {
     // default:  call Print(const size_t, const size_t)
     Print(10, 0);
   } else {
     // non-null option:  call PHTable::Print, for lack of
     // anything better to do ...
     PHTable::Print(option);
   }
}

void
dBbcGhitRawWrapper::SetMaxRowCount(const size_t& max_rows)
{
  // Avoid reallocing a space of zero size!
  if (max_rows <= 0) {
     return;
  }

  // Ensure that the current row count is not out of range.
  if ((size_t) fTableHeader->nok > max_rows) {
     fTableHeader->nok = max_rows;
  }

  // If table needs to grow, allocate a new area for it.
  if (max_rows > (size_t) fTableHeader->maxlen) {
     DBBCGHITRAW_ST* newData = new DBBCGHITRAW_ST[max_rows];
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
dBbcGhitRawWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dBbcGhitRawWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dBbcGhitRawWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dBbcGhitRawWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DBBCGHITRAW_ST)) {
       // Sanity check failed.  Need some error message here.
       return;
     }

     // Reallocate the table explicitly here; the size of the data array
     // may be inconsistent with the max. row count variable in the header
     // (since the ROOT I/O default-constructs the former, and reads
     // the header for the latter).
     size_t max_rows = MaxRowCount();
     if (max_rows <= 0) { // Avoid allocating a space of zero size!
        max_rows = 1;
     }

     delete [] fTableData;
     fTableData = new DBBCGHITRAW_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dBbcGhitRawWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].rawid;
        b >> fTableData[i].ghitid;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].rawid;
        b << fTableData[i].ghitid;
     }
   }

}
/* Automatically generated.  Do not edit. */
