#include <dCrkCalWrapper.h>

ClassImp(dCrkCalWrapper)

dCrkCalWrapper::dCrkCalWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DCRKCAL_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DCRKCAL_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DCRKCAL_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dCrkCal");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dCrkCalWrapper::~dCrkCalWrapper()
{
  delete [] fTableData;
}

void*
dCrkCalWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DCRKCAL_ST*
dCrkCalWrapper::TableData()
{
  return fTableData;
}

DCRKCAL_ST&
dCrkCalWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DCRKCAL_ST&
dCrkCalWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dCrkCalWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DCRKCAL_ST* newData = new DCRKCAL_ST[max_rows];
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
dCrkCalWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dCrkCalWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dCrkCalWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dCrkCalWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DCRKCAL_ST)) {
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
 	   fTableData = new DCRKCAL_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dCrkCalWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].pmt;
        b >> fTableData[i].adc_gain;
        b >> fTableData[i].adc_ped;
        b >> fTableData[i].tdc_clock;
        b >> fTableData[i].tdc_t0;
        b >> fTableData[i].slew;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].pmt;
        b << fTableData[i].adc_gain;
        b << fTableData[i].adc_ped;
        b << fTableData[i].tdc_clock;
        b << fTableData[i].tdc_t0;
        b << fTableData[i].slew;
     }
   }

}
/* Automatically generated.  Do not edit. */
