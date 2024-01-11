#include <cstring>
#include <iostream>
#include <iomanip>
#include "dTofUcalWrapper.h"

ClassImp(dTofUcalWrapper);

using namespace std;

dTofUcalWrapper::dTofUcalWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DTOFUCAL_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DTOFUCAL_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DTOFUCAL_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dTofUcal");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dTofUcalWrapper::dTofUcalWrapper(const dTofUcalWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DTOFUCAL_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DTOFUCAL_ST));
  SetType("dTofUcal");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dTofUcalWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dTofUcalWrapper&
dTofUcalWrapper::operator=(const dTofUcalWrapper& source)
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

dTofUcalWrapper::~dTofUcalWrapper()
{
  delete [] fTableData;
}

DTOFUCAL_ST*
dTofUcalWrapper::TableData()
{
  return fTableData;
}

DTOFUCAL_ST&
dTofUcalWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DTOFUCAL_ST&
dTofUcalWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dTofUcalWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "slatid";
  cout << " qvc_chgain[0]";
  cout << " qvc_chgain[1]";
  cout << " tvc_conv[0]";
  cout << " tvc_conv[1]";
  cout << " tvc_ped[0]";
  cout << " tvc_ped[1]";
  cout << " slew_a[0]";
  cout << " slew_a[1]";
  cout << " slew_b[0]";
  cout << " slew_b[1]";
  cout << " " << setw(11) << "scint_vlight";
  cout << " " << setw(11) << "scint_attenu";
  cout << " " << setw(11) << "tof_sigma";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].slatid;
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].qvc_chgain[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].tvc_conv[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].tvc_ped[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].slew_a[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].slew_b[i0];
     }
     cout << " " << setw(11) << fTableData[row].scint_vlight;
     cout << " " << setw(11) << fTableData[row].scint_attenu;
     cout << " " << setw(11) << fTableData[row].tof_sigma;

     cout << endl;
  }

}

void
dTofUcalWrapper::Print(Option_t* option) const
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
dTofUcalWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DTOFUCAL_ST* newData = new DTOFUCAL_ST[max_rows];
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
dTofUcalWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dTofUcalWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dTofUcalWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dTofUcalWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DTOFUCAL_ST)) {
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
     fTableData = new DTOFUCAL_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dTofUcalWrapper");

     for (unsigned int i=0; i<RowCount(); i++) {
        b >> fTableData[i].slatid;
        b.ReadStaticArray(fTableData[i].qvc_chgain);
        b.ReadStaticArray(fTableData[i].tvc_conv);
        b.ReadStaticArray(fTableData[i].tvc_ped);
        b.ReadStaticArray(fTableData[i].slew_a);
        b.ReadStaticArray(fTableData[i].slew_b);
        b >> fTableData[i].scint_vlight;
        b >> fTableData[i].scint_attenu;
        b >> fTableData[i].tof_sigma;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned int i=0; i<RowCount(); i++) {
        b << fTableData[i].slatid;
        b.WriteArray(fTableData[i].qvc_chgain,2);
        b.WriteArray(fTableData[i].tvc_conv,2);
        b.WriteArray(fTableData[i].tvc_ped,2);
        b.WriteArray(fTableData[i].slew_a,2);
        b.WriteArray(fTableData[i].slew_b,2);
        b << fTableData[i].scint_vlight;
        b << fTableData[i].scint_attenu;
        b << fTableData[i].tof_sigma;
     }
   }

}
/* Automatically generated.  Do not edit. */
