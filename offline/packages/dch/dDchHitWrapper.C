#include "dDchHitWrapper.h"
#include <cstring>

ClassImp(dDchHitWrapper)

dDchHitWrapper::dDchHitWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DDCHHIT_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DDCHHIT_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DDCHHIT_ST[1];
     SetMaxRowCount(1);
  }
  std::memset(fTableData,0,MaxRowCount()*sizeof(DDCHHIT_ST));
  SetRowSize(rowSize);
  SetType("dDchHit");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dDchHitWrapper::~dDchHitWrapper()
{
  delete [] fTableData;
}

void*
dDchHitWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DDCHHIT_ST*
dDchHitWrapper::TableData()
{
  return fTableData;
}

DDCHHIT_ST&
dDchHitWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DDCHHIT_ST&
dDchHitWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dDchHitWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DDCHHIT_ST* newData = new DDCHHIT_ST[max_rows];
     if (fTableData) {
        for (long i = 0; i < fTableHeader->nok; i++) {
           newData[i] = fTableData[i];
        }
        delete [] fTableData;
     }
     fTableData = newData;
     std::memset(fTableData,0,MaxRowCount()*sizeof(DDCHHIT_ST));
     fTableHeader->data_pointer = (long)fTableData;
  }

  fTableHeader->maxlen = max_rows;
}
void
dDchHitWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dDchHitWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dDchHitWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dDchHitWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DDCHHIT_ST)) {
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
 	   fTableData = new DDCHHIT_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dDchHitWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].arm;
        b >> fTableData[i].plane;
        b >> fTableData[i].cell;
        b >> fTableData[i].side;
        b >> fTableData[i].distance;
        b >> fTableData[i].width;
        b >> fTableData[i].time1;
        b >> fTableData[i].time2;
        b >> fTableData[i].idraw1;
        b >> fTableData[i].idraw2;
        b >> fTableData[i].idmirror;
        b >> fTableData[i].used;
        b.ReadStaticArray(fTableData[i].xyz);
        b.ReadStaticArray(fTableData[i].err);
        b.ReadStaticArray(fTableData[i].vxyz);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].arm;
        b << fTableData[i].plane;
        b << fTableData[i].cell;
        b << fTableData[i].side;
        b << fTableData[i].distance;
        b << fTableData[i].width;
        b << fTableData[i].time1;
        b << fTableData[i].time2;
        b << fTableData[i].idraw1;
        b << fTableData[i].idraw2;
        b << fTableData[i].idmirror;
        b << fTableData[i].used;
        b.WriteArray(fTableData[i].xyz,3);
        b.WriteArray(fTableData[i].err,3);
        b.WriteArray(fTableData[i].vxyz,3);
     }
   }

}
/* Automatically generated.  Do not edit. */
