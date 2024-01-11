#include <cstdlib>
#include <dPadDCMWrapper.h>

ClassImp(dPadDCMWrapper)

dPadDCMWrapper::dPadDCMWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DPADDCM_ST);
  if ((int) max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DPADDCM_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DPADDCM_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dPadDCM");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dPadDCMWrapper::~dPadDCMWrapper()
{
  delete [] fTableData;
}

void*
dPadDCMWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DPADDCM_ST*
dPadDCMWrapper::TableData()
{
  return fTableData;
}

DPADDCM_ST&
dPadDCMWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DPADDCM_ST&
dPadDCMWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dPadDCMWrapper::SetMaxRowCount(const size_t& max_rows)
{
  // Avoid reallocing a space of zero size!
  if ((int) max_rows <= 0) {
     return;
  }

  // Ensure that the current row count is not out of range.
  if (fTableHeader->nok > (int) max_rows) {
     fTableHeader->nok = max_rows;
  }

  // If table needs to grow, allocate a new area for it.
  if ((int) max_rows > fTableHeader->maxlen) {
     DPADDCM_ST* newData = new DPADDCM_ST[max_rows];
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
dPadDCMWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dPadDCMWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dPadDCMWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dPadDCMWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DPADDCM_ST)) {
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
 	   fTableData = new DPADDCM_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dPadDCMWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b.ReadStaticArray(fTableData[i].Word);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b.WriteArray(fTableData[i].Word,132);
     }
   }

}
/* Automatically generated.  Do not edit. */
