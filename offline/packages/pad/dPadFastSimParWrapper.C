#include <cstdlib>
#include <dPadFastSimParWrapper.h>

ClassImp(dPadFastSimParWrapper)

dPadFastSimParWrapper::dPadFastSimParWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DPADFASTSIMPAR_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DPADFASTSIMPAR_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DPADFASTSIMPAR_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dPadFastSimPar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dPadFastSimParWrapper::~dPadFastSimParWrapper()
{
  delete [] fTableData;
}

void*
dPadFastSimParWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DPADFASTSIMPAR_ST*
dPadFastSimParWrapper::TableData()
{
  return fTableData;
}

DPADFASTSIMPAR_ST&
dPadFastSimParWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DPADFASTSIMPAR_ST&
dPadFastSimParWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dPadFastSimParWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DPADFASTSIMPAR_ST* newData = new DPADFASTSIMPAR_ST[max_rows];
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
dPadFastSimParWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dPadFastSimParWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dPadFastSimParWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dPadFastSimParWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DPADFASTSIMPAR_ST)) {
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
 	   fTableData = new DPADFASTSIMPAR_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dPadFastSimParWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].pcnumber;
        b.ReadStaticArray(fTableData[i].randseed);
        b.ReadStaticArray(fTableData[i].efficiency);
        b.ReadStaticArray(fTableData[i].phires);
        b.ReadStaticArray(fTableData[i].zres);
        b.ReadStaticArray(fTableData[i].phisep);
        b.ReadStaticArray(fTableData[i].zsep);
        b >> fTableData[i].verbose;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].pcnumber;
        b.WriteArray(fTableData[i].randseed,3);
        b.WriteArray(fTableData[i].efficiency,3);
        b.WriteArray(fTableData[i].phires,3);
        b.WriteArray(fTableData[i].zres,3);
        b.WriteArray(fTableData[i].phisep,3);
        b.WriteArray(fTableData[i].zsep,3);
        b << fTableData[i].verbose;
     }
   }

}
/* Automatically generated.  Do not edit. */
