#include "dDchUnpackParWrapper.h"

ClassImp(dDchUnpackParWrapper)

dDchUnpackParWrapper::dDchUnpackParWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DDCHUNPACKPAR_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DDCHUNPACKPAR_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DDCHUNPACKPAR_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dDchUnpackPar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dDchUnpackParWrapper::~dDchUnpackParWrapper()
{
  delete [] fTableData;
}

void*
dDchUnpackParWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DDCHUNPACKPAR_ST*
dDchUnpackParWrapper::TableData()
{
  return fTableData;
}

DDCHUNPACKPAR_ST&
dDchUnpackParWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DDCHUNPACKPAR_ST&
dDchUnpackParWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dDchUnpackParWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DDCHUNPACKPAR_ST* newData = new DDCHUNPACKPAR_ST[max_rows];
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
dDchUnpackParWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dDchUnpackParWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dDchUnpackParWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dDchUnpackParWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DDCHUNPACKPAR_ST)) {
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
 	   fTableData = new DDCHUNPACKPAR_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dDchUnpackParWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].detIdWord;
        b >> fTableData[i].dc111;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].detIdWord;
        b << fTableData[i].dc111;
     }
   }

}
/* Automatically generated.  Do not edit. */
