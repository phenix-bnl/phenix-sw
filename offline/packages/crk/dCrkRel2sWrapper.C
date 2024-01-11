#include <dCrkRel2sWrapper.h>

ClassImp(dCrkRel2sWrapper)

dCrkRel2sWrapper::dCrkRel2sWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DCRKREL2S_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DCRKREL2S_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DCRKREL2S_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dCrkRel2s");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dCrkRel2sWrapper::~dCrkRel2sWrapper()
{
  delete [] fTableData;
}

void*
dCrkRel2sWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DCRKREL2S_ST*
dCrkRel2sWrapper::TableData()
{
  return fTableData;
}

DCRKREL2S_ST&
dCrkRel2sWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DCRKREL2S_ST&
dCrkRel2sWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dCrkRel2sWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DCRKREL2S_ST* newData = new DCRKREL2S_ST[max_rows];
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
dCrkRel2sWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dCrkRel2sWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dCrkRel2sWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dCrkRel2sWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DCRKREL2S_ST)) {
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
 	   fTableData = new DCRKREL2S_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dCrkRel2sWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id1;
        b >> fTableData[i].id2;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id1;
        b << fTableData[i].id2;
     }
   }

}
/* Automatically generated.  Do not edit. */
