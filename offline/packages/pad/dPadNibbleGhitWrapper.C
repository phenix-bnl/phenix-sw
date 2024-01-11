#include <cstdlib>
#include <dPadNibbleGhitWrapper.h>

ClassImp(dPadNibbleGhitWrapper)

dPadNibbleGhitWrapper::dPadNibbleGhitWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DPADNIBBLEGHIT_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DPADNIBBLEGHIT_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DPADNIBBLEGHIT_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dPadNibbleGhit");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dPadNibbleGhitWrapper::~dPadNibbleGhitWrapper()
{
  delete [] fTableData;
}

void*
dPadNibbleGhitWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DPADNIBBLEGHIT_ST*
dPadNibbleGhitWrapper::TableData()
{
  return fTableData;
}

DPADNIBBLEGHIT_ST&
dPadNibbleGhitWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DPADNIBBLEGHIT_ST&
dPadNibbleGhitWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dPadNibbleGhitWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DPADNIBBLEGHIT_ST* newData = new DPADNIBBLEGHIT_ST[max_rows];
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
dPadNibbleGhitWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dPadNibbleGhitWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dPadNibbleGhitWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dPadNibbleGhitWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DPADNIBBLEGHIT_ST)) {
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
 	   fTableData = new DPADNIBBLEGHIT_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dPadNibbleGhitWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].ghitid;
        b >> fTableData[i].rawid;
        b >> fTableData[i].Card;
        b >> fTableData[i].padx;
        b >> fTableData[i].padz;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].ghitid;
        b << fTableData[i].rawid;
        b << fTableData[i].Card;
        b << fTableData[i].padx;
        b << fTableData[i].padz;
     }
   }

}
/* Automatically generated.  Do not edit. */
