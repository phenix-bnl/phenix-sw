#include <cstring>
#include <iostream>
#include <iomanip>
#include "dTofUcalParWrapper.h"

ClassImp(dTofUcalParWrapper);

using namespace std;

dTofUcalParWrapper::dTofUcalParWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DTOFUCALPAR_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DTOFUCALPAR_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DTOFUCALPAR_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dTofUcalPar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dTofUcalParWrapper::dTofUcalParWrapper(const dTofUcalParWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DTOFUCALPAR_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DTOFUCALPAR_ST));
  SetType("dTofUcalPar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dTofUcalParWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dTofUcalParWrapper&
dTofUcalParWrapper::operator=(const dTofUcalParWrapper& source)
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

dTofUcalParWrapper::~dTofUcalParWrapper()
{
  delete [] fTableData;
}

DTOFUCALPAR_ST*
dTofUcalParWrapper::TableData()
{
  return fTableData;
}

DTOFUCALPAR_ST&
dTofUcalParWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DTOFUCALPAR_ST&
dTofUcalParWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dTofUcalParWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "option";
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
  cout << " datafile[0]";
  cout << " datafile[1]";
  cout << " datafile[2]";
  cout << " datafile[3]";
  cout << " datafile[4]";
  cout << " datafile[5]";
  cout << " datafile[6]";
  cout << " datafile[7]";
  cout << " datafile[8]";
  cout << " datafile[9]";
  cout << " datafile[10]";
  cout << " datafile[11]";
  cout << " datafile[12]";
  cout << " datafile[13]";
  cout << " datafile[14]";
  cout << " datafile[15]";
  cout << " datafile[16]";
  cout << " datafile[17]";
  cout << " datafile[18]";
  cout << " datafile[19]";
  cout << " datafile[20]";
  cout << " datafile[21]";
  cout << " datafile[22]";
  cout << " datafile[23]";
  cout << " datafile[24]";
  cout << " datafile[25]";
  cout << " datafile[26]";
  cout << " datafile[27]";
  cout << " datafile[28]";
  cout << " datafile[29]";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].option;
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
     for(int i0=0; i0<30; i0++) {
        cout << " " << setw(11) << fTableData[row].datafile[i0];
     }

     cout << endl;
  }

}

void
dTofUcalParWrapper::Print(Option_t* option) const
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
dTofUcalParWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DTOFUCALPAR_ST* newData = new DTOFUCALPAR_ST[max_rows];
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
dTofUcalParWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dTofUcalParWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dTofUcalParWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dTofUcalParWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DTOFUCALPAR_ST)) {
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
     fTableData = new DTOFUCALPAR_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dTofUcalParWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].option;
        b.ReadStaticArray(fTableData[i].qvc_chgain);
        b.ReadStaticArray(fTableData[i].tvc_conv);
        b.ReadStaticArray(fTableData[i].tvc_ped);
        b.ReadStaticArray(fTableData[i].slew_a);
        b.ReadStaticArray(fTableData[i].slew_b);
        b >> fTableData[i].scint_vlight;
        b >> fTableData[i].scint_attenu;
        b >> fTableData[i].tof_sigma;
        b.ReadStaticArray(fTableData[i].datafile);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].option;
        b.WriteArray(fTableData[i].qvc_chgain,2);
        b.WriteArray(fTableData[i].tvc_conv,2);
        b.WriteArray(fTableData[i].tvc_ped,2);
        b.WriteArray(fTableData[i].slew_a,2);
        b.WriteArray(fTableData[i].slew_b,2);
        b << fTableData[i].scint_vlight;
        b << fTableData[i].scint_attenu;
        b << fTableData[i].tof_sigma;
        b.WriteArray(fTableData[i].datafile,30);
     }
   }

}
/* Automatically generated.  Do not edit. */
