#include <cstring>
#include <iostream>
#include <iomanip>
#include "dDchTracksExtWrapper.h"

ClassImp(dDchTracksExtWrapper);

using namespace std;

dDchTracksExtWrapper::dDchTracksExtWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DDCHTRACKSEXT_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DDCHTRACKSEXT_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DDCHTRACKSEXT_ST[1];
     SetMaxRowCount(1);
  }

  memset(fTableData,0xFF,MaxRowCount()*sizeof(DDCHTRACKSEXT_ST));
  SetRowSize(rowSize);
  SetType("dDchTracksExt");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dDchTracksExtWrapper::dDchTracksExtWrapper(const dDchTracksExtWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DDCHTRACKSEXT_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DDCHTRACKSEXT_ST));
  SetType("dDchTracksExt");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dDchTracksExtWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dDchTracksExtWrapper&
dDchTracksExtWrapper::operator=(const dDchTracksExtWrapper& source)
{
  if (this != &source) {
     // The row count will be set by the PHTable assignment operator.

     PHTable::operator=(source);
     
     // Just copy the data from the source table.
     for (size_t row = 0; row < RowCount(); row++) {
        fTableData[row] = source.fTableData[row];
     }

  }

  return *this;
}

dDchTracksExtWrapper::~dDchTracksExtWrapper()
{
  delete [] fTableData;
}

DDCHTRACKSEXT_ST*
dDchTracksExtWrapper::TableData()
{
  return fTableData;
}

DDCHTRACKSEXT_ST&
dDchTracksExtWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DDCHTRACKSEXT_ST&
dDchTracksExtWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dDchTracksExtWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "id";
  cout << " " << setw(11) << "status";
  cout << " " << setw(11) << "alpha1";
  cout << " " << setw(11) << "alpha2";
  cout << " " << setw(11) << "dist1";
  cout << " " << setw(11) << "dist2";
  cout << " " << setw(11) << "chi21";
  cout << " " << setw(11) << "chi22";
  cout << " " << setw(11) << "nx1hits";
  cout << " " << setw(11) << "nx2hits";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].id;
     cout << " " << setw(11) << fTableData[row].status;
     cout << " " << setw(11) << fTableData[row].alpha1;
     cout << " " << setw(11) << fTableData[row].alpha2;
     cout << " " << setw(11) << fTableData[row].dist1;
     cout << " " << setw(11) << fTableData[row].dist2;
     cout << " " << setw(11) << fTableData[row].chi21;
     cout << " " << setw(11) << fTableData[row].chi22;
     cout << " " << setw(11) << fTableData[row].nx1hits;
     cout << " " << setw(11) << fTableData[row].nx2hits;

     cout << endl;
  }

}

void
dDchTracksExtWrapper::Print(Option_t* option) const
{
   // This version of Print overrides the one in the TObject
   // base class, and provides a way to call Print with no
   // arguments.  If Print(const size_t, const size_t) const
   // could be called with no arguments, there would be an
   // ambiguity.  I hope that this explanation makes sense!

   if (!option || (strlen(option) <= 0) ) {
     // default:  call Print(const size_t, const size_t)
     Print(10, 0);
   } else {
     // non-null option:  call PHTable::Print, for lack of
     // anything better to do ...
     PHTable::Print(option);
   }
}

void
dDchTracksExtWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DDCHTRACKSEXT_ST* newData = new DDCHTRACKSEXT_ST[max_rows];
     if (fTableData) {
        for (long i = 0; i < fTableHeader->nok; i++) {
           newData[i] = fTableData[i];
        }
        delete [] fTableData;
     }
     fTableData = newData;
     memset(fTableData,0xFF,MaxRowCount()*sizeof(DDCHTRACKSEXT_ST));
     fTableHeader->data_pointer = (long)fTableData;
  }

  fTableHeader->maxlen = max_rows;
}
void
dDchTracksExtWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dDchTracksExtWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dDchTracksExtWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dDchTracksExtWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DDCHTRACKSEXT_ST)) {
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
     fTableData = new DDCHTRACKSEXT_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dDchTracksExtWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].status;
        b >> fTableData[i].alpha1;
        b >> fTableData[i].alpha2;
        b >> fTableData[i].dist1;
        b >> fTableData[i].dist2;
        b >> fTableData[i].chi21;
        b >> fTableData[i].chi22;
        b >> fTableData[i].nx1hits;
        b >> fTableData[i].nx2hits;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].status;
        b << fTableData[i].alpha1;
        b << fTableData[i].alpha2;
        b << fTableData[i].dist1;
        b << fTableData[i].dist2;
        b << fTableData[i].chi21;
        b << fTableData[i].chi22;
        b << fTableData[i].nx1hits;
        b << fTableData[i].nx2hits;
     }
   }

}
/* Automatically generated.  Do not edit. */
