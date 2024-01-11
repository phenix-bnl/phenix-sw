#include <cstring>
#include <iostream>
#include <iomanip>
#include "dBbcOutWrapper.h"

using namespace std;

ClassImp(dBbcOutWrapper)

dBbcOutWrapper::dBbcOutWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DBBCOUT_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DBBCOUT_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DBBCOUT_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dBbcOut");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dBbcOutWrapper::dBbcOutWrapper(const dBbcOutWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DBBCOUT_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DBBCOUT_ST));
  SetType("dBbcOut");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dBbcOutWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dBbcOutWrapper&
dBbcOutWrapper::operator=(const dBbcOutWrapper& source)
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

dBbcOutWrapper::~dBbcOutWrapper()
{
  delete [] fTableData;
}

DBBCOUT_ST*
dBbcOutWrapper::TableData()
{
  return fTableData;
}

DBBCOUT_ST&
dBbcOutWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DBBCOUT_ST&
dBbcOutWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dBbcOutWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "NhitPmtNorth";
  cout << " " << setw(11) << "NhitPmtSouth";
  cout << " " << setw(11) << "ChargeSumNorth";
  cout << " " << setw(11) << "ChargeSumSouth";
  cout << " " << setw(11) << "VertexPoint";
  cout << " " << setw(11) << "dVertexPoint";
  cout << " " << setw(11) << "TimeZero";
  cout << " " << setw(11) << "dTimeZero";
  cout << " Adc[0]";
  cout << " Adc[1]";
  cout << " Adc[2]";
  cout << " Adc[3]";
  cout << " Adc[4]";
  cout << " Adc[5]";
  cout << " Adc[6]";
  cout << " Adc[7]";
  cout << " Adc[8]";
  cout << " Adc[9]";
  cout << " Adc[10]";
  cout << " Adc[11]";
  cout << " Adc[12]";
  cout << " Adc[13]";
  cout << " Adc[14]";
  cout << " Adc[15]";
  cout << " Adc[16]";
  cout << " Adc[17]";
  cout << " Adc[18]";
  cout << " Adc[19]";
  cout << " Adc[20]";
  cout << " Adc[21]";
  cout << " Adc[22]";
  cout << " Adc[23]";
  cout << " Adc[24]";
  cout << " Adc[25]";
  cout << " Adc[26]";
  cout << " Adc[27]";
  cout << " Adc[28]";
  cout << " Adc[29]";
  cout << " Adc[30]";
  cout << " Adc[31]";
  cout << " Adc[32]";
  cout << " Adc[33]";
  cout << " Adc[34]";
  cout << " Adc[35]";
  cout << " Adc[36]";
  cout << " Adc[37]";
  cout << " Adc[38]";
  cout << " Adc[39]";
  cout << " Adc[40]";
  cout << " Adc[41]";
  cout << " Adc[42]";
  cout << " Adc[43]";
  cout << " Adc[44]";
  cout << " Adc[45]";
  cout << " Adc[46]";
  cout << " Adc[47]";
  cout << " Adc[48]";
  cout << " Adc[49]";
  cout << " Adc[50]";
  cout << " Adc[51]";
  cout << " Adc[52]";
  cout << " Adc[53]";
  cout << " Adc[54]";
  cout << " Adc[55]";
  cout << " Adc[56]";
  cout << " Adc[57]";
  cout << " Adc[58]";
  cout << " Adc[59]";
  cout << " Adc[60]";
  cout << " Adc[61]";
  cout << " Adc[62]";
  cout << " Adc[63]";
  cout << " Adc[64]";
  cout << " Adc[65]";
  cout << " Adc[66]";
  cout << " Adc[67]";
  cout << " Adc[68]";
  cout << " Adc[69]";
  cout << " Adc[70]";
  cout << " Adc[71]";
  cout << " Adc[72]";
  cout << " Adc[73]";
  cout << " Adc[74]";
  cout << " Adc[75]";
  cout << " Adc[76]";
  cout << " Adc[77]";
  cout << " Adc[78]";
  cout << " Adc[79]";
  cout << " Adc[80]";
  cout << " Adc[81]";
  cout << " Adc[82]";
  cout << " Adc[83]";
  cout << " Adc[84]";
  cout << " Adc[85]";
  cout << " Adc[86]";
  cout << " Adc[87]";
  cout << " Adc[88]";
  cout << " Adc[89]";
  cout << " Adc[90]";
  cout << " Adc[91]";
  cout << " Adc[92]";
  cout << " Adc[93]";
  cout << " Adc[94]";
  cout << " Adc[95]";
  cout << " Adc[96]";
  cout << " Adc[97]";
  cout << " Adc[98]";
  cout << " Adc[99]";
  cout << " Adc[100]";
  cout << " Adc[101]";
  cout << " Adc[102]";
  cout << " Adc[103]";
  cout << " Adc[104]";
  cout << " Adc[105]";
  cout << " Adc[106]";
  cout << " Adc[107]";
  cout << " Adc[108]";
  cout << " Adc[109]";
  cout << " Adc[110]";
  cout << " Adc[111]";
  cout << " Adc[112]";
  cout << " Adc[113]";
  cout << " Adc[114]";
  cout << " Adc[115]";
  cout << " Adc[116]";
  cout << " Adc[117]";
  cout << " Adc[118]";
  cout << " Adc[119]";
  cout << " Adc[120]";
  cout << " Adc[121]";
  cout << " Adc[122]";
  cout << " Adc[123]";
  cout << " Adc[124]";
  cout << " Adc[125]";
  cout << " Adc[126]";
  cout << " Adc[127]";
  cout << " Tdc[0]";
  cout << " Tdc[1]";
  cout << " Tdc[2]";
  cout << " Tdc[3]";
  cout << " Tdc[4]";
  cout << " Tdc[5]";
  cout << " Tdc[6]";
  cout << " Tdc[7]";
  cout << " Tdc[8]";
  cout << " Tdc[9]";
  cout << " Tdc[10]";
  cout << " Tdc[11]";
  cout << " Tdc[12]";
  cout << " Tdc[13]";
  cout << " Tdc[14]";
  cout << " Tdc[15]";
  cout << " Tdc[16]";
  cout << " Tdc[17]";
  cout << " Tdc[18]";
  cout << " Tdc[19]";
  cout << " Tdc[20]";
  cout << " Tdc[21]";
  cout << " Tdc[22]";
  cout << " Tdc[23]";
  cout << " Tdc[24]";
  cout << " Tdc[25]";
  cout << " Tdc[26]";
  cout << " Tdc[27]";
  cout << " Tdc[28]";
  cout << " Tdc[29]";
  cout << " Tdc[30]";
  cout << " Tdc[31]";
  cout << " Tdc[32]";
  cout << " Tdc[33]";
  cout << " Tdc[34]";
  cout << " Tdc[35]";
  cout << " Tdc[36]";
  cout << " Tdc[37]";
  cout << " Tdc[38]";
  cout << " Tdc[39]";
  cout << " Tdc[40]";
  cout << " Tdc[41]";
  cout << " Tdc[42]";
  cout << " Tdc[43]";
  cout << " Tdc[44]";
  cout << " Tdc[45]";
  cout << " Tdc[46]";
  cout << " Tdc[47]";
  cout << " Tdc[48]";
  cout << " Tdc[49]";
  cout << " Tdc[50]";
  cout << " Tdc[51]";
  cout << " Tdc[52]";
  cout << " Tdc[53]";
  cout << " Tdc[54]";
  cout << " Tdc[55]";
  cout << " Tdc[56]";
  cout << " Tdc[57]";
  cout << " Tdc[58]";
  cout << " Tdc[59]";
  cout << " Tdc[60]";
  cout << " Tdc[61]";
  cout << " Tdc[62]";
  cout << " Tdc[63]";
  cout << " Tdc[64]";
  cout << " Tdc[65]";
  cout << " Tdc[66]";
  cout << " Tdc[67]";
  cout << " Tdc[68]";
  cout << " Tdc[69]";
  cout << " Tdc[70]";
  cout << " Tdc[71]";
  cout << " Tdc[72]";
  cout << " Tdc[73]";
  cout << " Tdc[74]";
  cout << " Tdc[75]";
  cout << " Tdc[76]";
  cout << " Tdc[77]";
  cout << " Tdc[78]";
  cout << " Tdc[79]";
  cout << " Tdc[80]";
  cout << " Tdc[81]";
  cout << " Tdc[82]";
  cout << " Tdc[83]";
  cout << " Tdc[84]";
  cout << " Tdc[85]";
  cout << " Tdc[86]";
  cout << " Tdc[87]";
  cout << " Tdc[88]";
  cout << " Tdc[89]";
  cout << " Tdc[90]";
  cout << " Tdc[91]";
  cout << " Tdc[92]";
  cout << " Tdc[93]";
  cout << " Tdc[94]";
  cout << " Tdc[95]";
  cout << " Tdc[96]";
  cout << " Tdc[97]";
  cout << " Tdc[98]";
  cout << " Tdc[99]";
  cout << " Tdc[100]";
  cout << " Tdc[101]";
  cout << " Tdc[102]";
  cout << " Tdc[103]";
  cout << " Tdc[104]";
  cout << " Tdc[105]";
  cout << " Tdc[106]";
  cout << " Tdc[107]";
  cout << " Tdc[108]";
  cout << " Tdc[109]";
  cout << " Tdc[110]";
  cout << " Tdc[111]";
  cout << " Tdc[112]";
  cout << " Tdc[113]";
  cout << " Tdc[114]";
  cout << " Tdc[115]";
  cout << " Tdc[116]";
  cout << " Tdc[117]";
  cout << " Tdc[118]";
  cout << " Tdc[119]";
  cout << " Tdc[120]";
  cout << " Tdc[121]";
  cout << " Tdc[122]";
  cout << " Tdc[123]";
  cout << " Tdc[124]";
  cout << " Tdc[125]";
  cout << " Tdc[126]";
  cout << " Tdc[127]";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].NhitPmtNorth;
     cout << " " << setw(11) << fTableData[row].NhitPmtSouth;
     cout << " " << setw(11) << fTableData[row].ChargeSumNorth;
     cout << " " << setw(11) << fTableData[row].ChargeSumSouth;
     cout << " " << setw(11) << fTableData[row].VertexPoint;
     cout << " " << setw(11) << fTableData[row].dVertexPoint;
     cout << " " << setw(11) << fTableData[row].TimeZero;
     cout << " " << setw(11) << fTableData[row].dTimeZero;
     for(int i0=0; i0<128; i0++) {
        cout << " " << setw(11) << fTableData[row].Adc[i0];
     }
     for(int i0=0; i0<128; i0++) {
        cout << " " << setw(11) << fTableData[row].Tdc[i0];
     }

     cout << endl;
  }

}

void
dBbcOutWrapper::Print(Option_t* option) const
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
dBbcOutWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DBBCOUT_ST* newData = new DBBCOUT_ST[max_rows];
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
dBbcOutWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dBbcOutWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dBbcOutWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dBbcOutWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DBBCOUT_ST)) {
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
     fTableData = new DBBCOUT_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dBbcOutWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].NhitPmtNorth;
        b >> fTableData[i].NhitPmtSouth;
        b >> fTableData[i].ChargeSumNorth;
        b >> fTableData[i].ChargeSumSouth;
        b >> fTableData[i].VertexPoint;
        b >> fTableData[i].dVertexPoint;
        b >> fTableData[i].TimeZero;
        b >> fTableData[i].dTimeZero;
        b.ReadStaticArray(fTableData[i].Adc);
        b.ReadStaticArray(fTableData[i].Tdc);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].NhitPmtNorth;
        b << fTableData[i].NhitPmtSouth;
        b << fTableData[i].ChargeSumNorth;
        b << fTableData[i].ChargeSumSouth;
        b << fTableData[i].VertexPoint;
        b << fTableData[i].dVertexPoint;
        b << fTableData[i].TimeZero;
        b << fTableData[i].dTimeZero;
        b.WriteArray(fTableData[i].Adc,128);
        b.WriteArray(fTableData[i].Tdc,128);
     }
   }

}
/* Automatically generated.  Do not edit. */
