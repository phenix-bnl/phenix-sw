#include <cstring>
#include <iostream>
#include <iomanip>
#include "dTofGdigiWrapper.h"

ClassImp(dTofGdigiWrapper);

using namespace std;

dTofGdigiWrapper::dTofGdigiWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DTOFGDIGI_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DTOFGDIGI_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DTOFGDIGI_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dTofGdigi");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dTofGdigiWrapper::dTofGdigiWrapper(const dTofGdigiWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DTOFGDIGI_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DTOFGDIGI_ST));
  SetType("dTofGdigi");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dTofGdigiWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dTofGdigiWrapper&
dTofGdigiWrapper::operator=(const dTofGdigiWrapper& source)
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

dTofGdigiWrapper::~dTofGdigiWrapper()
{
  delete [] fTableData;
}

DTOFGDIGI_ST*
dTofGdigiWrapper::TableData()
{
  return fTableData;
}

DTOFGDIGI_ST&
dTofGdigiWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DTOFGDIGI_ST&
dTofGdigiWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dTofGdigiWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "id";
  cout << " " << setw(11) << "slatid";
  cout << " " << setw(11) << "panel";
  cout << " " << setw(11) << "column";
  cout << " " << setw(11) << "pslat";
  cout << " " << setw(11) << "slat_seq";
  cout << " " << setw(11) << "mctrack";
  cout << " " << setw(11) << "partl";
  cout << " " << setw(11) << "tof";
  cout << " " << setw(11) << "eloss";
  cout << " pos_m[0]";
  cout << " pos_m[1]";
  cout << " pos_m[2]";
  cout << " " << setw(11) << "pos_hit_slat";
  cout << " " << setw(11) << "theta";
  cout << " " << setw(11) << "phi";
  cout << " p_m[0]";
  cout << " p_m[1]";
  cout << " p_m[2]";
  cout << " " << setw(11) << "path";
  cout << " " << setw(11) << "nslathit";
  cout << " " << setw(11) << "hits_seq";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].id;
     cout << " " << setw(11) << fTableData[row].slatid;
     cout << " " << setw(11) << fTableData[row].panel;
     cout << " " << setw(11) << fTableData[row].column;
     cout << " " << setw(11) << fTableData[row].pslat;
     cout << " " << setw(11) << fTableData[row].slat_seq;
     cout << " " << setw(11) << fTableData[row].mctrack;
     cout << " " << setw(11) << fTableData[row].partl;
     cout << " " << setw(11) << fTableData[row].tof;
     cout << " " << setw(11) << fTableData[row].eloss;
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].pos_m[i0];
     }
     cout << " " << setw(11) << fTableData[row].pos_hit_slat;
     cout << " " << setw(11) << fTableData[row].theta;
     cout << " " << setw(11) << fTableData[row].phi;
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].p_m[i0];
     }
     cout << " " << setw(11) << fTableData[row].path;
     cout << " " << setw(11) << fTableData[row].nslathit;
     cout << " " << setw(11) << fTableData[row].hits_seq;

     cout << endl;
  }

}

void
dTofGdigiWrapper::Print(Option_t* option) const
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
dTofGdigiWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DTOFGDIGI_ST* newData = new DTOFGDIGI_ST[max_rows];
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
dTofGdigiWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dTofGdigiWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dTofGdigiWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dTofGdigiWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DTOFGDIGI_ST)) {
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
     fTableData = new DTOFGDIGI_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dTofGdigiWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].slatid;
        b >> fTableData[i].panel;
        b >> fTableData[i].column;
        b >> fTableData[i].pslat;
        b >> fTableData[i].slat_seq;
        b >> fTableData[i].mctrack;
        b >> fTableData[i].partl;
        b >> fTableData[i].tof;
        b >> fTableData[i].eloss;
        b.ReadStaticArray(fTableData[i].pos_m);
        b >> fTableData[i].pos_hit_slat;
        b >> fTableData[i].theta;
        b >> fTableData[i].phi;
        b.ReadStaticArray(fTableData[i].p_m);
        b >> fTableData[i].path;
        b >> fTableData[i].nslathit;
        b >> fTableData[i].hits_seq;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].slatid;
        b << fTableData[i].panel;
        b << fTableData[i].column;
        b << fTableData[i].pslat;
        b << fTableData[i].slat_seq;
        b << fTableData[i].mctrack;
        b << fTableData[i].partl;
        b << fTableData[i].tof;
        b << fTableData[i].eloss;
        b.WriteArray(fTableData[i].pos_m,3);
        b << fTableData[i].pos_hit_slat;
        b << fTableData[i].theta;
        b << fTableData[i].phi;
        b.WriteArray(fTableData[i].p_m,3);
        b << fTableData[i].path;
        b << fTableData[i].nslathit;
        b << fTableData[i].hits_seq;
     }
   }

}
/* Automatically generated.  Do not edit. */
