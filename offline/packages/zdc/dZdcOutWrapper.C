#include <cstring>
#include <iostream>
#include <iomanip>
#include <dZdcOutWrapper.h>

using namespace std;

ClassImp(dZdcOutWrapper)

dZdcOutWrapper::dZdcOutWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DZDCOUT_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DZDCOUT_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DZDCOUT_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dZdcOut");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dZdcOutWrapper::dZdcOutWrapper(const dZdcOutWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DZDCOUT_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DZDCOUT_ST));
  SetType("dZdcOut");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dZdcOutWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dZdcOutWrapper&
dZdcOutWrapper::operator=(const dZdcOutWrapper& source)
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

dZdcOutWrapper::~dZdcOutWrapper()
{
  delete [] fTableData;
}

DZDCOUT_ST*
dZdcOutWrapper::TableData()
{
  return fTableData;
}

DZDCOUT_ST&
dZdcOutWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DZDCOUT_ST&
dZdcOutWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dZdcOutWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " Charge[0]";
  cout << " Charge[1]";
  cout << " Charge[2]";
  cout << " Charge[3]";
  cout << " Charge[4]";
  cout << " Charge[5]";
  cout << " Charge[6]";
  cout << " Charge[7]";
  cout << " Timing0[0]";
  cout << " Timing0[1]";
  cout << " Timing0[2]";
  cout << " Timing0[3]";
  cout << " Timing0[4]";
  cout << " Timing0[5]";
  cout << " Timing0[6]";
  cout << " Timing0[7]";
  cout << " Timing1[0]";
  cout << " Timing1[1]";
  cout << " Timing1[2]";
  cout << " Timing1[3]";
  cout << " Timing1[4]";
  cout << " Timing1[5]";
  cout << " Timing1[6]";
  cout << " Timing1[7]";
  cout << " Energy[0]";
  cout << " Energy[1]";
  cout << " Timing[0]";
  cout << " Timing[1]";
  cout << " " << setw(11) << "Zvertex";
  cout << " " << setw(11) << "ZvertexError";
  cout << " " << setw(11) << "TimeZero";
  cout << " " << setw(11) << "timeZeroError";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     for(int i0=0; i0<8; i0++) {
        cout << " " << setw(11) << fTableData[row].Charge[i0];
     }
     for(int i0=0; i0<8; i0++) {
        cout << " " << setw(11) << fTableData[row].Timing0[i0];
     }
     for(int i0=0; i0<8; i0++) {
        cout << " " << setw(11) << fTableData[row].Timing1[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].Energy[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].Timing[i0];
     }
     cout << " " << setw(11) << fTableData[row].Zvertex;
     cout << " " << setw(11) << fTableData[row].ZvertexError;
     cout << " " << setw(11) << fTableData[row].TimeZero;
     cout << " " << setw(11) << fTableData[row].timeZeroError;

     cout << endl;
  }

}

void
dZdcOutWrapper::Print(Option_t* option) const
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
dZdcOutWrapper::SetMaxRowCount(const size_t& max_rows)
{
  // Avoid reallocing a space of zero size!
  if (max_rows <= 0) {
     return;
  }

  // Ensure that the current row count is not out of range.
  if (fTableHeader->nok > (int)max_rows) {
     fTableHeader->nok = (int)max_rows;
  }

  // If table needs to grow, allocate a new area for it.
  if (max_rows > (unsigned int)fTableHeader->maxlen) {
     DZDCOUT_ST* newData = new DZDCOUT_ST[max_rows];
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
dZdcOutWrapper::SetRowCount(const size_t& n)
{
  if (n > (unsigned int)fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n > 0) {
     fTableHeader->nok = n;
  }
}

void
dZdcOutWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dZdcOutWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dZdcOutWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DZDCOUT_ST)) {
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
     fTableData = new DZDCOUT_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dZdcOutWrapper");

     for (unsigned int i=0; i<RowCount(); i++) {
        b.ReadStaticArray(fTableData[i].Charge);
        b.ReadStaticArray(fTableData[i].Timing0);
        b.ReadStaticArray(fTableData[i].Timing1);
        b.ReadStaticArray(fTableData[i].Energy);
        b.ReadStaticArray(fTableData[i].Timing);
        b >> fTableData[i].Zvertex;
        b >> fTableData[i].ZvertexError;
        b >> fTableData[i].TimeZero;
        b >> fTableData[i].timeZeroError;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned int i=0; i<RowCount(); i++) {
        b.WriteArray(fTableData[i].Charge,8);
        b.WriteArray(fTableData[i].Timing0,8);
        b.WriteArray(fTableData[i].Timing1,8);
        b.WriteArray(fTableData[i].Energy,2);
        b.WriteArray(fTableData[i].Timing,2);
        b << fTableData[i].Zvertex;
        b << fTableData[i].ZvertexError;
        b << fTableData[i].TimeZero;
        b << fTableData[i].timeZeroError;
     }
   }

}
/* Automatically generated.  Do not edit. */
