//INCLUDECHECKER: Removed this line: #include <cstdlib>
#include "headerWrapper.h"

ClassImp(headerWrapper)

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
  if ((size_t)fTableHeader->nok > max_rows) {
     fTableHeader->nok = max_rows;
  }

  // If table needs to grow, allocate a new area for it.
  if (max_rows > (size_t)fTableHeader->maxlen) {
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
  if (n > (size_t)fTableHeader->maxlen) {
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

void
headerWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class headerWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     //Version_t v = b.ReadVersion();
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(HEADER_ST)) {
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
 	   fTableData = new HEADER_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("headerWrapper");

     for (long i=0; i<(long)RowCount(); i++) {
        b >> fTableData[i].run;
        b >> fTableData[i].event;
        b >> fTableData[i].multiplicity;
        b >> fTableData[i].b;
        b >> fTableData[i].a1;
        b >> fTableData[i].z1;
        b >> fTableData[i].a2;
        b >> fTableData[i].z2;
        b >> fTableData[i].sqrt_s;
        b >> fTableData[i].bmin;
        b >> fTableData[i].bmax;
        b >> fTableData[i].t0femto;
        b.ReadStaticArray(fTableData[i].vertex);
        b >> fTableData[i].itime;
        b >> fTableData[i].idate;
        b.ReadStaticArray(fTableData[i].nrndm);
        b >> fTableData[i].isqStart;
        b >> fTableData[i].iSeconds;
        b >> fTableData[i].maxTrueTrack;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (long i=0; i<(long)RowCount(); i++) {
        b << fTableData[i].run;
        b << fTableData[i].event;
        b << fTableData[i].multiplicity;
        b << fTableData[i].b;
        b << fTableData[i].a1;
        b << fTableData[i].z1;
        b << fTableData[i].a2;
        b << fTableData[i].z2;
        b << fTableData[i].sqrt_s;
        b << fTableData[i].bmin;
        b << fTableData[i].bmax;
        b << fTableData[i].t0femto;
        b.WriteArray(fTableData[i].vertex,3);
        b << fTableData[i].itime;
        b << fTableData[i].idate;
        b.WriteArray(fTableData[i].nrndm,2);
        b << fTableData[i].isqStart;
        b << fTableData[i].iSeconds;
        b << fTableData[i].maxTrueTrack;
     }
   }

}
/* Automatically generated.  Do not edit. */
