#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iomanip>
#include "dTofEvtHeaderWrapper.h"

ClassImp(dTofEvtHeaderWrapper);

using namespace std;

dTofEvtHeaderWrapper::dTofEvtHeaderWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DTOFEVTHEADER_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DTOFEVTHEADER_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DTOFEVTHEADER_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dTofEvtHeader");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dTofEvtHeaderWrapper::dTofEvtHeaderWrapper(const dTofEvtHeaderWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DTOFEVTHEADER_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DTOFEVTHEADER_ST));
  SetType("dTofEvtHeader");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dTofEvtHeaderWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dTofEvtHeaderWrapper&
dTofEvtHeaderWrapper::operator=(const dTofEvtHeaderWrapper& source)
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

dTofEvtHeaderWrapper::~dTofEvtHeaderWrapper()
{
  delete [] fTableData;
}

DTOFEVTHEADER_ST*
dTofEvtHeaderWrapper::TableData()
{
  return fTableData;
}

DTOFEVTHEADER_ST&
dTofEvtHeaderWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DTOFEVTHEADER_ST&
dTofEvtHeaderWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dTofEvtHeaderWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "run";
  cout << " " << setw(11) << "date";
  cout << " " << setw(11) << "time";
  cout << " " << setw(11) << "evtseq";
  cout << " " << setw(11) << "scaledtrig";
  cout << " " << setw(11) << "rawtrig";
  cout << " " << setw(11) << "livetrig";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].run;
     cout << " " << setw(11) << fTableData[row].date;
     cout << " " << setw(11) << fTableData[row].time;
     cout << " " << setw(11) << fTableData[row].evtseq;
     cout << " " << setw(11) << fTableData[row].scaledtrig;
     cout << " " << setw(11) << fTableData[row].rawtrig;
     cout << " " << setw(11) << fTableData[row].livetrig;

     cout << endl;
  }

}

void
dTofEvtHeaderWrapper::Print(Option_t* option)
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
dTofEvtHeaderWrapper::SetMaxRowCount(const size_t& max_rows)
{
  // Avoid reallocing a space of zero size!
  if (max_rows <= 0) {
     return;
  }

  // Ensure that the current row count is not out of range.
  if (fTableHeader->nok > max_rows) {
     fTableHeader->nok = max_rows;
  }

  // If table needs to grow, allocate a new area for it.
  if (max_rows > fTableHeader->maxlen) {
     DTOFEVTHEADER_ST* newData = new DTOFEVTHEADER_ST[max_rows];
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
dTofEvtHeaderWrapper::SetRowCount(const size_t& n)
{
  if (n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dTofEvtHeaderWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dTofEvtHeaderWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dTofEvtHeaderWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     //Version_t v = b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DTOFEVTHEADER_ST)) {
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
     fTableData = new DTOFEVTHEADER_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dTofEvtHeaderWrapper");

     for (long i=0; i<RowCount(); i++) {
        b >> fTableData[i].run;
        b >> fTableData[i].date;
        b >> fTableData[i].time;
        b >> fTableData[i].evtseq;
        b >> fTableData[i].scaledtrig;
        b >> fTableData[i].rawtrig;
        b >> fTableData[i].livetrig;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (long i=0; i<RowCount(); i++) {
        b << fTableData[i].run;
        b << fTableData[i].date;
        b << fTableData[i].time;
        b << fTableData[i].evtseq;
        b << fTableData[i].scaledtrig;
        b << fTableData[i].rawtrig;
        b << fTableData[i].livetrig;
     }
   }

}
/* Automatically generated.  Do not edit. */
