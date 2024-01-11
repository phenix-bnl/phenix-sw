#include "dEmcDCMDataWrapper.h"

ClassImp(dEmcDCMDataWrapper)

dEmcDCMDataWrapper::dEmcDCMDataWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DEMCDCMDATA_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DEMCDCMDATA_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DEMCDCMDATA_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dEmcDCMData");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dEmcDCMDataWrapper::~dEmcDCMDataWrapper()
{
  delete [] fTableData;
}

void*
dEmcDCMDataWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DEMCDCMDATA_ST*
dEmcDCMDataWrapper::TableData()
{
  return fTableData;
}

DEMCDCMDATA_ST&
dEmcDCMDataWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DEMCDCMDATA_ST&
dEmcDCMDataWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dEmcDCMDataWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DEMCDCMDATA_ST* newData = new DEMCDCMDATA_ST[max_rows];
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
dEmcDCMDataWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  } else {
     fTableHeader->nok = n;
  }
}

void
dEmcDCMDataWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dEmcDCMDataWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dEmcDCMDataWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DEMCDCMDATA_ST)) {
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
 	   fTableData = new DEMCDCMDATA_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dEmcDCMDataWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].nWords;
        b >> fTableData[i].scheme;
        b >> fTableData[i].packetID;
        b.ReadStaticArray(fTableData[i].DCM);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].nWords;
        b << fTableData[i].scheme;
        b << fTableData[i].packetID;
        b.WriteArray(fTableData[i].DCM,450);
     }
   }

}
/* Automatically generated.  Do not edit. */
