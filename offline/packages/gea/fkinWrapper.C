//INCLUDECHECKER: Removed this line: #include <cstdlib>
#include "fkinWrapper.h"

ClassImp(fkinWrapper)

fkinWrapper::fkinWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(FKIN_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new FKIN_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new FKIN_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("fkin");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

fkinWrapper::~fkinWrapper()
{
  delete [] fTableData;
}

void*
fkinWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

FKIN_ST*
fkinWrapper::TableData()
{
  return fTableData;
}

FKIN_ST&
fkinWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const FKIN_ST&
fkinWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
fkinWrapper::SetMaxRowCount(const size_t& max_rows)
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
     FKIN_ST* newData = new FKIN_ST[max_rows];
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
fkinWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t)fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
fkinWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
fkinWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class fkinWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     //Version_t v = b.ReadVersion();
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(FKIN_ST)) {
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
 	   fTableData = new FKIN_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("fkinWrapper");

     for (long i=0; i<(long)RowCount(); i++) {
        b >> fTableData[i].true_track;
        b >> fTableData[i].subevent;
        b >> fTableData[i].ntrack;
        b >> fTableData[i].ptot;
        b >> fTableData[i].pthet;
        b >> fTableData[i].pphi;
        b >> fTableData[i].r_vertex;
        b >> fTableData[i].z_vertex;
        b >> fTableData[i].th_vertx;
        b >> fTableData[i].ph_vertx;
        b >> fTableData[i].itparent;
        b >> fTableData[i].idparent;
        b >> fTableData[i].idpart;
        b >> fTableData[i].nfile;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (long i=0; i<(long)RowCount(); i++) {
        b << fTableData[i].true_track;
        b << fTableData[i].subevent;
        b << fTableData[i].ntrack;
        b << fTableData[i].ptot;
        b << fTableData[i].pthet;
        b << fTableData[i].pphi;
        b << fTableData[i].r_vertex;
        b << fTableData[i].z_vertex;
        b << fTableData[i].th_vertx;
        b << fTableData[i].ph_vertx;
        b << fTableData[i].itparent;
        b << fTableData[i].idparent;
        b << fTableData[i].idpart;
        b << fTableData[i].nfile;
     }
   }

}
/* Automatically generated.  Do not edit. */
