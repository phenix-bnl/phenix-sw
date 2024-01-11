#include "dEmcEventWrapper.h"

ClassImp(dEmcEventWrapper)

dEmcEventWrapper::dEmcEventWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DEMCEVENT_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DEMCEVENT_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DEMCEVENT_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dEmcEvent");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dEmcEventWrapper::~dEmcEventWrapper()
{
  delete [] fTableData;
}

void*
dEmcEventWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DEMCEVENT_ST*
dEmcEventWrapper::TableData()
{
  return fTableData;
}

DEMCEVENT_ST&
dEmcEventWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DEMCEVENT_ST&
dEmcEventWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dEmcEventWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DEMCEVENT_ST* newData = new DEMCEVENT_ST[max_rows];
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
dEmcEventWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  } else {
     fTableHeader->nok = n;
  }
}

void
dEmcEventWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dEmcEventWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dEmcEventWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DEMCEVENT_ST)) {
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
 	   fTableData = new DEMCEVENT_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dEmcEventWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].evtyp;
        b >> fTableData[i].evno;
        b >> fTableData[i].runno;
        b >> fTableData[i].serialno;
        b >> fTableData[i].impact;
        b.ReadStaticArray(fTableData[i].xyz);
        b >> fTableData[i].twrmultlo;
        b >> fTableData[i].twrmulthi;
        b >> fTableData[i].tote;
        b >> fTableData[i].totet;
        b.ReadStaticArray(fTableData[i].trigsum);
        b.ReadStaticArray(fTableData[i].sece);
        b.ReadStaticArray(fTableData[i].secet);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].evtyp;
        b << fTableData[i].evno;
        b << fTableData[i].runno;
        b << fTableData[i].serialno;
        b << fTableData[i].impact;
        b.WriteArray(fTableData[i].xyz,3);
        b << fTableData[i].twrmultlo;
        b << fTableData[i].twrmulthi;
        b << fTableData[i].tote;
        b << fTableData[i].totet;
        b.WriteArray(fTableData[i].trigsum,3);
        b.WriteArray(fTableData[i].sece,8);
        b.WriteArray(fTableData[i].secet,8);
     }
   }

}
/* Automatically generated.  Do not edit. */
