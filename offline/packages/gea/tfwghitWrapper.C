#include <cstdlib>
#include "tfwghitWrapper.h"

ClassImp(tfwghitWrapper)

tfwghitWrapper::tfwghitWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(TFWGHIT_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new TFWGHIT_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new TFWGHIT_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("tfwghit");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

tfwghitWrapper::~tfwghitWrapper()
{
  delete [] fTableData;
}

void*
tfwghitWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

TFWGHIT_ST*
tfwghitWrapper::TableData()
{
  return fTableData;
}

TFWGHIT_ST&
tfwghitWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const TFWGHIT_ST&
tfwghitWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
tfwghitWrapper::SetMaxRowCount(const size_t& max_rows)
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
     TFWGHIT_ST* newData = new TFWGHIT_ST[max_rows];
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
tfwghitWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
tfwghitWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
tfwghitWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class tfwghitWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(TFWGHIT_ST)) {
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
 	   fTableData = new TFWGHIT_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("tfwghitWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b.ReadStaticArray(fTableData[i].xyzinglo);
        b.ReadStaticArray(fTableData[i].xyzinloc);
        b.ReadStaticArray(fTableData[i].xyzoutloc);
        b >> fTableData[i].tof;
        b >> fTableData[i].pathLength;
        b >> fTableData[i].dele;
        b >> fTableData[i].panel;
        b >> fTableData[i].idpart;
        b >> fTableData[i].mctrack;
        b >> fTableData[i].nFile;
	b >> fTableData[i].track;
	b >> fTableData[i].isubevent;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b.WriteArray(fTableData[i].xyzinglo,3);
        b.WriteArray(fTableData[i].xyzinloc,3);
        b.WriteArray(fTableData[i].xyzoutloc,3);
        b <<  fTableData[i].tof;
        b << fTableData[i].pathLength;
        b << fTableData[i].dele;
        b << fTableData[i].panel;
        b << fTableData[i].idpart;
        b << fTableData[i].mctrack;
        b << fTableData[i].nFile;
	b << fTableData[i].track;
	b << fTableData[i].isubevent;
     }
   }

}
/* Automatically generated.  Do not edit. */
