#include "dDchTracksWrapper.h"
#include <cstring>

ClassImp(dDchTracksWrapper)

dDchTracksWrapper::dDchTracksWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DDCHTRACKS_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DDCHTRACKS_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DDCHTRACKS_ST[1];
     SetMaxRowCount(1);
  }

  std::memset(fTableData,0xFF,MaxRowCount()*sizeof(DDCHTRACKS_ST));
  SetRowSize(rowSize);
  SetType("dDchTracks");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dDchTracksWrapper::~dDchTracksWrapper()
{
  delete [] fTableData;
}

void*
dDchTracksWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DDCHTRACKS_ST*
dDchTracksWrapper::TableData()
{
  return fTableData;
}

DDCHTRACKS_ST&
dDchTracksWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DDCHTRACKS_ST&
dDchTracksWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dDchTracksWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DDCHTRACKS_ST* newData = new DDCHTRACKS_ST[max_rows];
     if (fTableData) {
        for (long i = 0; i < fTableHeader->nok; i++) {
           newData[i] = fTableData[i];
        }
        delete [] fTableData;
     }
     fTableData = newData;
     std::memset(fTableData,0xFF,MaxRowCount()*sizeof(DDCHTRACKS_ST));
     fTableHeader->data_pointer = (long)fTableData;
  }

  fTableHeader->maxlen = max_rows;
}
void
dDchTracksWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dDchTracksWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dDchTracksWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dDchTracksWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DDCHTRACKS_ST)) {
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
 	   fTableData = new DDCHTRACKS_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dDchTracksWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].trackid;
        b >> fTableData[i].arm;
        b >> fTableData[i].side;
        b.ReadStaticArray(fTableData[i].point);
        b.ReadStaticArray(fTableData[i].err_point);
        b.ReadStaticArray(fTableData[i].direction);
        b.ReadStaticArray(fTableData[i].err_direction);
        b.ReadStaticArray(fTableData[i].hits);
        b >> fTableData[i].quality;
        b >> fTableData[i].phi;
        b >> fTableData[i].alpha;
        b >> fTableData[i].beta;
        b >> fTableData[i].betaNoVertex;
        b >> fTableData[i].zed;
        b >> fTableData[i].phi0;
        b >> fTableData[i].theta0;
        b >> fTableData[i].momentum;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].trackid;
        b << fTableData[i].arm;
        b << fTableData[i].side;
        b.WriteArray(fTableData[i].point,3);
        b.WriteArray(fTableData[i].err_point,3);
        b.WriteArray(fTableData[i].direction,3);
        b.WriteArray(fTableData[i].err_direction,3);
        b.WriteArray(fTableData[i].hits,40);
        b << fTableData[i].quality;
        b << fTableData[i].phi;
        b << fTableData[i].alpha;
        b << fTableData[i].beta;
        b << fTableData[i].betaNoVertex;
        b << fTableData[i].zed;
        b << fTableData[i].phi0;
        b << fTableData[i].theta0;
        b << fTableData[i].momentum;
     }
   }

}
/* Automatically generated.  Do not edit. */
