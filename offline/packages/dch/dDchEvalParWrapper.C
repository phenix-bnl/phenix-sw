#include "dDchEvalParWrapper.h"

ClassImp(dDchEvalParWrapper)

dDchEvalParWrapper::dDchEvalParWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DDCHEVALPAR_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DDCHEVALPAR_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DDCHEVALPAR_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dDchEvalPar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dDchEvalParWrapper::~dDchEvalParWrapper()
{
  delete [] fTableData;
}

void*
dDchEvalParWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DDCHEVALPAR_ST*
dDchEvalParWrapper::TableData()
{
  return fTableData;
}

DDCHEVALPAR_ST&
dDchEvalParWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DDCHEVALPAR_ST&
dDchEvalParWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dDchEvalParWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DDCHEVALPAR_ST* newData = new DDCHEVALPAR_ST[max_rows];
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
dDchEvalParWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dDchEvalParWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dDchEvalParWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dDchEvalParWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DDCHEVALPAR_ST)) {
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
 	   fTableData = new DDCHEVALPAR_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dDchEvalParWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].verbose;
        b >> fTableData[i].coordinates;
        b >> fTableData[i].main;
        b >> fTableData[i].effiMainCut;
        b >> fTableData[i].effiDphiCut;
        b >> fTableData[i].effiDalphaCut;
        b >> fTableData[i].effiDzedCut;
        b >> fTableData[i].ghostMainCut;
        b >> fTableData[i].ghostDphiCut;
        b >> fTableData[i].ghostDalphaCut;
        b >> fTableData[i].ghostDzedCut;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].verbose;
        b << fTableData[i].coordinates;
        b << fTableData[i].main;
        b << fTableData[i].effiMainCut;
        b << fTableData[i].effiDphiCut;
        b << fTableData[i].effiDalphaCut;
        b << fTableData[i].effiDzedCut;
        b << fTableData[i].ghostMainCut;
        b << fTableData[i].ghostDphiCut;
        b << fTableData[i].ghostDalphaCut;
        b << fTableData[i].ghostDzedCut;
     }
   }

}
/* Automatically generated.  Do not edit. */
