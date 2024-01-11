#include "pc23parWrapper.h"

ClassImp(pc23parWrapper)

pc23parWrapper::pc23parWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(PC23PAR_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new PC23PAR_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new PC23PAR_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("pc23par");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

pc23parWrapper::~pc23parWrapper()
{
  delete [] fTableData;
}

void*
pc23parWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

PC23PAR_ST*
pc23parWrapper::TableData()
{
  return fTableData;
}

PC23PAR_ST&
pc23parWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const PC23PAR_ST&
pc23parWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
pc23parWrapper::SetMaxRowCount(const size_t& max_rows)
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
     PC23PAR_ST* newData = new PC23PAR_ST[max_rows];
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
pc23parWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
pc23parWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
pc23parWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class pc23parWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(PC23PAR_ST)) {
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
 	   fTableData = new PC23PAR_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("pc23parWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].idatePC23;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].idatePC23;
     }
   }

}
/* Automatically generated.  Do not edit. */
