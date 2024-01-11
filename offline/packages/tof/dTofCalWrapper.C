#include <cstring>
#include <iostream>
#include <iomanip>
#include "dTofCalWrapper.h"

ClassImp(dTofCalWrapper);

using namespace std;

dTofCalWrapper::dTofCalWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DTOFCAL_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DTOFCAL_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DTOFCAL_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dTofCal");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dTofCalWrapper::dTofCalWrapper(const dTofCalWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DTOFCAL_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DTOFCAL_ST));
  SetType("dTofCal");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dTofCalWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dTofCalWrapper&
dTofCalWrapper::operator=(const dTofCalWrapper& source)
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

dTofCalWrapper::~dTofCalWrapper()
{
  delete [] fTableData;
}

DTOFCAL_ST*
dTofCalWrapper::TableData()
{
  return fTableData;
}

DTOFCAL_ST&
dTofCalWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DTOFCAL_ST&
dTofCalWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dTofCalWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "slatid";
  cout << " qvc_corr[0]";
  cout << " qvc_corr[1]";
  cout << " qvc_corrlsr[0]";
  cout << " qvc_corrlsr[1]";
  cout << " " << setw(11) << "eloss_conv";
  cout << " " << setw(11) << "eloss_mip";
  cout << " tvc_conv[0]";
  cout << " tvc_conv[1]";
  cout << " tvc_ped[0]";
  cout << " tvc_ped[1]";
  cout << " t0[0]";
  cout << " t0[1]";
  cout << " t0_lsr[0]";
  cout << " t0_lsr[1]";
  cout << " slew_a[0]";
  cout << " slew_a[1]";
  cout << " slew_b[0]";
  cout << " slew_b[1]";
  cout << " " << setw(11) << "scint_vlight";
  cout << " " << setw(11) << "scint_attenu";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].slatid;
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].qvc_corr[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].qvc_corrlsr[i0];
     }
     cout << " " << setw(11) << fTableData[row].eloss_conv;
     cout << " " << setw(11) << fTableData[row].eloss_mip;
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].tvc_conv[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].tvc_ped[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].t0[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].t0_lsr[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].slew_a[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].slew_b[i0];
     }
     cout << " " << setw(11) << fTableData[row].scint_vlight;
     cout << " " << setw(11) << fTableData[row].scint_attenu;

     cout << endl;
  }

}

void
dTofCalWrapper::Print(Option_t* option) const
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
dTofCalWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DTOFCAL_ST* newData = new DTOFCAL_ST[max_rows];
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
dTofCalWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dTofCalWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dTofCalWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dTofCalWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DTOFCAL_ST)) {
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
     fTableData = new DTOFCAL_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dTofCalWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].slatid;
        b.ReadStaticArray(fTableData[i].qvc_corr);
        b.ReadStaticArray(fTableData[i].qvc_corrlsr);
        b >> fTableData[i].eloss_conv;
        b >> fTableData[i].eloss_mip;
        b.ReadStaticArray(fTableData[i].tvc_conv);
        b.ReadStaticArray(fTableData[i].tvc_ped);
        b.ReadStaticArray(fTableData[i].t0);
        b.ReadStaticArray(fTableData[i].t0_lsr);
        b.ReadStaticArray(fTableData[i].slew_a);
        b.ReadStaticArray(fTableData[i].slew_b);
        b >> fTableData[i].scint_vlight;
        b >> fTableData[i].scint_attenu;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].slatid;
        b.WriteArray(fTableData[i].qvc_corr,2);
        b.WriteArray(fTableData[i].qvc_corrlsr,2);
        b << fTableData[i].eloss_conv;
        b << fTableData[i].eloss_mip;
        b.WriteArray(fTableData[i].tvc_conv,2);
        b.WriteArray(fTableData[i].tvc_ped,2);
        b.WriteArray(fTableData[i].t0,2);
        b.WriteArray(fTableData[i].t0_lsr,2);
        b.WriteArray(fTableData[i].slew_a,2);
        b.WriteArray(fTableData[i].slew_b,2);
        b << fTableData[i].scint_vlight;
        b << fTableData[i].scint_attenu;
     }
   }

}
/* Automatically generated.  Do not edit. */
