#include <cstdlib>
#include <dPadFEMParWrapper.h>

ClassImp(dPadFEMParWrapper)

dPadFEMParWrapper::dPadFEMParWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DPADFEMPAR_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DPADFEMPAR_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DPADFEMPAR_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dPadFEMPar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dPadFEMParWrapper::~dPadFEMParWrapper()
{
  delete [] fTableData;
}

void*
dPadFEMParWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DPADFEMPAR_ST*
dPadFEMParWrapper::TableData()
{
  return fTableData;
}

DPADFEMPAR_ST&
dPadFEMParWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DPADFEMPAR_ST&
dPadFEMParWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dPadFEMParWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DPADFEMPAR_ST* newData = new DPADFEMPAR_ST[max_rows];
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
dPadFEMParWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dPadFEMParWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dPadFEMParWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dPadFEMParWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DPADFEMPAR_ST)) {
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
 	   fTableData = new DPADFEMPAR_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dPadFEMParWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].pcnumber;
        b >> fTableData[i].mode;
        b >> fTableData[i].debug;
        b >> fTableData[i].fout;
        b >> fTableData[i].skipg;
        b >> fTableData[i].last;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].pcnumber;
        b << fTableData[i].mode;
        b << fTableData[i].debug;
        b << fTableData[i].fout;
        b << fTableData[i].skipg;
        b << fTableData[i].last;
     }
   }

}
/* Automatically generated.  Do not edit. */
