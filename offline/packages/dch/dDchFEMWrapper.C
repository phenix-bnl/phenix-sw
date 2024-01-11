#include "dDchFEMWrapper.h"

ClassImp(dDchFEMWrapper)

dDchFEMWrapper::dDchFEMWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DDCHFEM_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DDCHFEM_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DDCHFEM_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dDchFEM");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dDchFEMWrapper::~dDchFEMWrapper()
{
  delete [] fTableData;
}

void*
dDchFEMWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DDCHFEM_ST*
dDchFEMWrapper::TableData()
{
  return fTableData;
}

DDCHFEM_ST&
dDchFEMWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DDCHFEM_ST&
dDchFEMWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dDchFEMWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DDCHFEM_ST* newData = new DDCHFEM_ST[max_rows];
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
dDchFEMWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dDchFEMWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dDchFEMWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dDchFEMWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DDCHFEM_ST)) {
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
 	   fTableData = new DDCHFEM_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dDchFEMWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].CAV1;
        b >> fTableData[i].det;
        b >> fTableData[i].Ecounter;
        b >> fTableData[i].adr;
        b >> fTableData[i].Flag;
        b >> fTableData[i].Bcounter;
        b.ReadStaticArray(fTableData[i].Word);
        b >> fTableData[i].usr1;
        b >> fTableData[i].usr2;
        b >> fTableData[i].usr3;
        b >> fTableData[i].usr4;
        b >> fTableData[i].parity;
        b >> fTableData[i].CAV2;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].CAV1;
        b << fTableData[i].det;
        b << fTableData[i].Ecounter;
        b << fTableData[i].adr;
        b << fTableData[i].Flag;
        b << fTableData[i].Bcounter;
        b.WriteArray(fTableData[i].Word,960);
        b << fTableData[i].usr1;
        b << fTableData[i].usr2;
        b << fTableData[i].usr3;
        b << fTableData[i].usr4;
        b << fTableData[i].parity;
        b << fTableData[i].CAV2;
     }
   }

}
/* Automatically generated.  Do not edit. */
