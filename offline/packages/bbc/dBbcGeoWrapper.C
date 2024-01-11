#include <cstring>
#include <iostream>
#include <iomanip>
#include "dBbcGeoWrapper.h"

using namespace std;

ClassImp(dBbcGeoWrapper)

dBbcGeoWrapper::dBbcGeoWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DBBCGEO_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DBBCGEO_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DBBCGEO_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dBbcGeo");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dBbcGeoWrapper::dBbcGeoWrapper(const dBbcGeoWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DBBCGEO_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DBBCGEO_ST));
  SetType("dBbcGeo");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dBbcGeoWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dBbcGeoWrapper&
dBbcGeoWrapper::operator=(const dBbcGeoWrapper& source)
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

dBbcGeoWrapper::~dBbcGeoWrapper()
{
  delete [] fTableData;
}

DBBCGEO_ST*
dBbcGeoWrapper::TableData()
{
  return fTableData;
}

DBBCGEO_ST&
dBbcGeoWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DBBCGEO_ST&
dBbcGeoWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dBbcGeoWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "MaxPmtNo";
  cout << " Ring[0]";
  cout << " Ring[1]";
  cout << " Ring[2]";
  cout << " Ring[3]";
  cout << " Ring[4]";
  cout << " Ring[5]";
  cout << " Ring[6]";
  cout << " Ring[7]";
  cout << " Ring[8]";
  cout << " Ring[9]";
  cout << " Ring[10]";
  cout << " Ring[11]";
  cout << " Ring[12]";
  cout << " Ring[13]";
  cout << " Ring[14]";
  cout << " Ring[15]";
  cout << " Ring[16]";
  cout << " Ring[17]";
  cout << " Ring[18]";
  cout << " Ring[19]";
  cout << " Ring[20]";
  cout << " Ring[21]";
  cout << " Ring[22]";
  cout << " Ring[23]";
  cout << " Ring[24]";
  cout << " Ring[25]";
  cout << " Ring[26]";
  cout << " Ring[27]";
  cout << " Ring[28]";
  cout << " Ring[29]";
  cout << " Ring[30]";
  cout << " Ring[31]";
  cout << " Ring[32]";
  cout << " Ring[33]";
  cout << " Ring[34]";
  cout << " Ring[35]";
  cout << " Ring[36]";
  cout << " Ring[37]";
  cout << " Ring[38]";
  cout << " Ring[39]";
  cout << " Ring[40]";
  cout << " Ring[41]";
  cout << " Ring[42]";
  cout << " Ring[43]";
  cout << " Ring[44]";
  cout << " Ring[45]";
  cout << " Ring[46]";
  cout << " Ring[47]";
  cout << " Ring[48]";
  cout << " Ring[49]";
  cout << " Ring[50]";
  cout << " Ring[51]";
  cout << " Ring[52]";
  cout << " Ring[53]";
  cout << " Ring[54]";
  cout << " Ring[55]";
  cout << " Ring[56]";
  cout << " Ring[57]";
  cout << " Ring[58]";
  cout << " Ring[59]";
  cout << " Ring[60]";
  cout << " Ring[61]";
  cout << " Ring[62]";
  cout << " Ring[63]";
  cout << " PMT[0]";
  cout << " PMT[1]";
  cout << " PMT[2]";
  cout << " PMT[3]";
  cout << " PMT[4]";
  cout << " PMT[5]";
  cout << " PMT[6]";
  cout << " PMT[7]";
  cout << " PMT[8]";
  cout << " PMT[9]";
  cout << " PMT[10]";
  cout << " PMT[11]";
  cout << " PMT[12]";
  cout << " PMT[13]";
  cout << " PMT[14]";
  cout << " PMT[15]";
  cout << " PMT[16]";
  cout << " PMT[17]";
  cout << " PMT[18]";
  cout << " PMT[19]";
  cout << " PMT[20]";
  cout << " PMT[21]";
  cout << " PMT[22]";
  cout << " PMT[23]";
  cout << " PMT[24]";
  cout << " PMT[25]";
  cout << " PMT[26]";
  cout << " PMT[27]";
  cout << " PMT[28]";
  cout << " PMT[29]";
  cout << " PMT[30]";
  cout << " PMT[31]";
  cout << " PMT[32]";
  cout << " PMT[33]";
  cout << " PMT[34]";
  cout << " PMT[35]";
  cout << " PMT[36]";
  cout << " PMT[37]";
  cout << " PMT[38]";
  cout << " PMT[39]";
  cout << " PMT[40]";
  cout << " PMT[41]";
  cout << " PMT[42]";
  cout << " PMT[43]";
  cout << " PMT[44]";
  cout << " PMT[45]";
  cout << " PMT[46]";
  cout << " PMT[47]";
  cout << " PMT[48]";
  cout << " PMT[49]";
  cout << " PMT[50]";
  cout << " PMT[51]";
  cout << " PMT[52]";
  cout << " PMT[53]";
  cout << " PMT[54]";
  cout << " PMT[55]";
  cout << " PMT[56]";
  cout << " PMT[57]";
  cout << " PMT[58]";
  cout << " PMT[59]";
  cout << " PMT[60]";
  cout << " PMT[61]";
  cout << " PMT[62]";
  cout << " PMT[63]";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].MaxPmtNo;
     for(int i0=0; i0<64; i0++) {
        cout << " " << setw(11) << fTableData[row].Ring[i0];
     }
     for(int i0=0; i0<64; i0++) {
        cout << " " << setw(11) << fTableData[row].PMT[i0];
     }

     cout << endl;
  }

}

void
dBbcGeoWrapper::Print(Option_t* option) const
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
dBbcGeoWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DBBCGEO_ST* newData = new DBBCGEO_ST[max_rows];
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
dBbcGeoWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dBbcGeoWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dBbcGeoWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dBbcGeoWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DBBCGEO_ST)) {
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
     fTableData = new DBBCGEO_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dBbcGeoWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].MaxPmtNo;
        b.ReadStaticArray(fTableData[i].Ring);
        b.ReadStaticArray(fTableData[i].PMT);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].MaxPmtNo;
        b.WriteArray(fTableData[i].Ring,64);
        b.WriteArray(fTableData[i].PMT,64);
     }
   }

}
/* Automatically generated.  Do not edit. */
