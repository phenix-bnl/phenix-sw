#include <cstring>
#include <iostream>
#include <iomanip>
#include "dTofPidParWrapper.h"

ClassImp(dTofPidParWrapper);

using namespace std;

dTofPidParWrapper::dTofPidParWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DTOFPIDPAR_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DTOFPIDPAR_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DTOFPIDPAR_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dTofPidPar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dTofPidParWrapper::dTofPidParWrapper(const dTofPidParWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DTOFPIDPAR_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DTOFPIDPAR_ST));
  SetType("dTofPidPar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dTofPidParWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dTofPidParWrapper&
dTofPidParWrapper::operator=(const dTofPidParWrapper& source)
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

dTofPidParWrapper::~dTofPidParWrapper()
{
  delete [] fTableData;
}

DTOFPIDPAR_ST*
dTofPidParWrapper::TableData()
{
  return fTableData;
}

DTOFPIDPAR_ST&
dTofPidParWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DTOFPIDPAR_ST&
dTofPidParWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dTofPidParWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "pid";
  cout << " " << setw(11) << "m2mean";
  cout << " cm2[0]";
  cout << " cm2[1]";
  cout << " cm2[2]";
  cout << " " << setw(11) << "m2min";
  cout << " " << setw(11) << "m2max";
  cout << " " << setw(11) << "pmin";
  cout << " " << setw(11) << "pmax";
  cout << " " << setw(11) << "charge";
  cout << " " << setw(11) << "factor";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].pid;
     cout << " " << setw(11) << fTableData[row].m2mean;
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].cm2[i0];
     }
     cout << " " << setw(11) << fTableData[row].m2min;
     cout << " " << setw(11) << fTableData[row].m2max;
     cout << " " << setw(11) << fTableData[row].pmin;
     cout << " " << setw(11) << fTableData[row].pmax;
     cout << " " << setw(11) << fTableData[row].charge;
     cout << " " << setw(11) << fTableData[row].factor;

     cout << endl;
  }

}

void
dTofPidParWrapper::Print(Option_t* option) const
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
dTofPidParWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DTOFPIDPAR_ST* newData = new DTOFPIDPAR_ST[max_rows];
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
dTofPidParWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dTofPidParWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dTofPidParWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dTofPidParWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DTOFPIDPAR_ST)) {
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
     fTableData = new DTOFPIDPAR_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dTofPidParWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].pid;
        b >> fTableData[i].m2mean;
        b.ReadStaticArray(fTableData[i].cm2);
        b >> fTableData[i].m2min;
        b >> fTableData[i].m2max;
        b >> fTableData[i].pmin;
        b >> fTableData[i].pmax;
        b >> fTableData[i].charge;
        b >> fTableData[i].factor;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].pid;
        b << fTableData[i].m2mean;
        b.WriteArray(fTableData[i].cm2,3);
        b << fTableData[i].m2min;
        b << fTableData[i].m2max;
        b << fTableData[i].pmin;
        b << fTableData[i].pmax;
        b << fTableData[i].charge;
        b << fTableData[i].factor;
     }
   }

}
/* Automatically generated.  Do not edit. */
