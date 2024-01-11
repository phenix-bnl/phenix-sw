#include <cstring>
#include <iostream>
#include <iomanip>
#include "dTofReconstructedWrapper.h"

ClassImp(dTofReconstructedWrapper);

using namespace std;

dTofReconstructedWrapper::dTofReconstructedWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DTOFRECONSTRUCTED_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DTOFRECONSTRUCTED_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DTOFRECONSTRUCTED_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dTofReconstructed");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dTofReconstructedWrapper::dTofReconstructedWrapper(const dTofReconstructedWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DTOFRECONSTRUCTED_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DTOFRECONSTRUCTED_ST));
  SetType("dTofReconstructed");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dTofReconstructedWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dTofReconstructedWrapper&
dTofReconstructedWrapper::operator=(const dTofReconstructedWrapper& source)
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

dTofReconstructedWrapper::~dTofReconstructedWrapper()
{
  delete [] fTableData;
}

DTOFRECONSTRUCTED_ST*
dTofReconstructedWrapper::TableData()
{
  return fTableData;
}

DTOFRECONSTRUCTED_ST&
dTofReconstructedWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DTOFRECONSTRUCTED_ST&
dTofReconstructedWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dTofReconstructedWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "id";
  cout << " " << setw(11) << "slatid";
  cout << " " << setw(11) << "sector";
  cout << " " << setw(11) << "side";
  cout << " " << setw(11) << "panel";
  cout << " " << setw(11) << "slat";
  cout << " " << setw(11) << "tof";
  cout << " " << setw(11) << "tof_err";
  cout << " " << setw(11) << "eloss";
  cout << " " << setw(11) << "eloss_err";
  cout << " xtof[0]";
  cout << " xtof[1]";
  cout << " xtof[2]";
  cout << " xtof_err[0]";
  cout << " xtof_err[1]";
  cout << " xtof_err[2]";
  cout << " qvc[0]";
  cout << " qvc[1]";
  cout << " tvc[0]";
  cout << " tvc[1]";
  cout << " tdiff";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].id;
     cout << " " << setw(11) << fTableData[row].slatid;
     cout << " " << setw(11) << fTableData[row].sector;
     cout << " " << setw(11) << fTableData[row].side;
     cout << " " << setw(11) << fTableData[row].panel;
     cout << " " << setw(11) << fTableData[row].slat;
     cout << " " << setw(11) << fTableData[row].tof;
     cout << " " << setw(11) << fTableData[row].tof_err;
     cout << " " << setw(11) << fTableData[row].eloss;
     cout << " " << setw(11) << fTableData[row].eloss_err;
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].xtof[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].xtof_err[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].qvc[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].tvc[i0];
     }
     cout << " " << setw(11) << fTableData[row].tdiff;

     cout << endl;
  }

}

void
dTofReconstructedWrapper::Print(Option_t* option) const
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
dTofReconstructedWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DTOFRECONSTRUCTED_ST* newData = new DTOFRECONSTRUCTED_ST[max_rows];
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
dTofReconstructedWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = (int) n;
  }
}

void
dTofReconstructedWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dTofReconstructedWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dTofReconstructedWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DTOFRECONSTRUCTED_ST)) {
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
     fTableData = new DTOFRECONSTRUCTED_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dTofReconstructedWrapper");

     for (unsigned int i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].slatid;
        b >> fTableData[i].sector;
        b >> fTableData[i].side;
        b >> fTableData[i].panel;
        b >> fTableData[i].slat;
        b >> fTableData[i].tof;
        b >> fTableData[i].tof_err;
        b >> fTableData[i].eloss;
        b >> fTableData[i].eloss_err;
        b.ReadStaticArray(fTableData[i].xtof);
        b.ReadStaticArray(fTableData[i].xtof_err);
        b.ReadStaticArray(fTableData[i].qvc);
        b.ReadStaticArray(fTableData[i].tvc);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned  i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].slatid;
        b << fTableData[i].sector;
        b << fTableData[i].side;
        b << fTableData[i].panel;
        b << fTableData[i].slat;
        b << fTableData[i].tof;
        b << fTableData[i].tof_err;
        b << fTableData[i].eloss;
        b << fTableData[i].eloss_err;
        b.WriteArray(fTableData[i].xtof,3);
        b.WriteArray(fTableData[i].xtof_err,3);
        b.WriteArray(fTableData[i].qvc,2);
        b.WriteArray(fTableData[i].tvc,2);
     }
   }

}
/* Automatically generated.  Do not edit. */
