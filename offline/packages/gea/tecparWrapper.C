#include "tecparWrapper.h"

ClassImp(tecparWrapper)

tecparWrapper::tecparWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(TECPAR_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new TECPAR_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new TECPAR_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("tecpar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

tecparWrapper::~tecparWrapper()
{
  delete [] fTableData;
}

void*
tecparWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

TECPAR_ST*
tecparWrapper::TableData()
{
  return fTableData;
}

TECPAR_ST&
tecparWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const TECPAR_ST&
tecparWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
tecparWrapper::SetMaxRowCount(const size_t& max_rows)
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
     TECPAR_ST* newData = new TECPAR_ST[max_rows];
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
tecparWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
tecparWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
tecparWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class tecparWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(TECPAR_ST)) {
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
 	   fTableData = new TECPAR_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("tecparWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].r0;
        b >> fTableData[i].angle;
        b >> fTableData[i].thRad;
        b >> fTableData[i].thXe;
        b >> fTableData[i].nArms;
        b >> fTableData[i].nSect;
        b >> fTableData[i].iArmor;
        b >> fTableData[i].NTEC;
        b.ReadStaticArray(fTableData[i].lTec);
        b >> fTableData[i].iDateTEC;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].r0;
        b << fTableData[i].angle;
        b << fTableData[i].thRad;
        b << fTableData[i].thXe;
        b << fTableData[i].nArms;
        b << fTableData[i].nSect;
        b << fTableData[i].iArmor;
        b << fTableData[i].NTEC;
        b.WriteArray(fTableData[i].lTec,6);
        b << fTableData[i].iDateTEC;
     }
   }

}
/* Automatically generated.  Do not edit. */
