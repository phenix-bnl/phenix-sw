//INCLUDECHECKER: Removed this line: #include <cstdlib>
#include "pythiaWrapper.h"

ClassImp(pythiaWrapper)

pythiaWrapper::pythiaWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(PYTHIA_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new PYTHIA_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new PYTHIA_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("pythia");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

pythiaWrapper::~pythiaWrapper()
{
  delete [] fTableData;
}

void*
pythiaWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

PYTHIA_ST*
pythiaWrapper::TableData()
{
  return fTableData;
}

PYTHIA_ST&
pythiaWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const PYTHIA_ST&
pythiaWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
pythiaWrapper::SetMaxRowCount(const size_t& max_rows)
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
     PYTHIA_ST* newData = new PYTHIA_ST[max_rows];
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
pythiaWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t)fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
pythiaWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
pythiaWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class pythiaWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     //Version_t v = b.ReadVersion();
     b.ReadVersion(); // suppress warning about unused variable
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(PYTHIA_ST)) {
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
 	   fTableData = new PYTHIA_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("pythiaWrapper");

     for (long i=0; i<(long)RowCount(); i++) {
        b >> fTableData[i].pyth_proc_id;
        b.ReadStaticArray(fTableData[i].pyth_bjork);
        b.ReadStaticArray(fTableData[i].pyth_parstu);
        b >> fTableData[i].pyth_qsqr;
        b >> fTableData[i].pyth_ptrans;
        b.ReadStaticArray(fTableData[i].intr_part_id);
        b.ReadStaticArray((float *)fTableData[i].intr_part_p);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (long i=0; i<(long)RowCount(); i++) {
        b << fTableData[i].pyth_proc_id;
        b.WriteArray(fTableData[i].pyth_bjork,2);
        b.WriteArray(fTableData[i].pyth_parstu,3);
        b << fTableData[i].pyth_qsqr;
        b << fTableData[i].pyth_ptrans;
        b.WriteArray(fTableData[i].intr_part_id,4);
        b.WriteArray((float *)fTableData[i].intr_part_p,16);
     }
   }

}

