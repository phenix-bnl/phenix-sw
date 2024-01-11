#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iomanip>
#include "dErtDcmDataWrapper.h"

ClassImp(dErtDcmDataWrapper);

using namespace std;

dErtDcmDataWrapper::dErtDcmDataWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DERTDCMDATA_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DERTDCMDATA_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DERTDCMDATA_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dErtDcmData");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dErtDcmDataWrapper::dErtDcmDataWrapper(const dErtDcmDataWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DERTDCMDATA_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DERTDCMDATA_ST));
  SetType("dErtDcmData");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dErtDcmDataWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dErtDcmDataWrapper&
dErtDcmDataWrapper::operator=(const dErtDcmDataWrapper& source)
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

dErtDcmDataWrapper::~dErtDcmDataWrapper()
{
  delete [] fTableData;
}

DERTDCMDATA_ST*
dErtDcmDataWrapper::TableData()
{
  return fTableData;
}

DERTDCMDATA_ST&
dErtDcmDataWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DERTDCMDATA_ST&
dErtDcmDataWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dErtDcmDataWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "Nwords";
  cout << " " << setw(11) << "packetID";
  cout << " " << setw(11) << "hitformat";
  cout << " word[0]";
  cout << " word[1]";
  cout << " word[2]";
  cout << " word[3]";
  cout << " word[4]";
  cout << " word[5]";
  cout << " word[6]";
  cout << " word[7]";
  cout << " word[8]";
  cout << " word[9]";
  cout << " word[10]";
  cout << " word[11]";
  cout << " word[12]";
  cout << " word[13]";
  cout << " word[14]";
  cout << " word[15]";
  cout << " word[16]";
  cout << " word[17]";
  cout << " word[18]";
  cout << " word[19]";
  cout << " word[20]";
  cout << " word[21]";
  cout << " word[22]";
  cout << " word[23]";
  cout << " word[24]";
  cout << " word[25]";
  cout << " word[26]";
  cout << " word[27]";
  cout << " word[28]";
  cout << " word[29]";
  cout << " word[30]";
  cout << " word[31]";
  cout << " word[32]";
  cout << " word[33]";
  cout << " word[34]";
  cout << " word[35]";
  cout << " word[36]";
  cout << " word[37]";
  cout << " word[38]";
  cout << " word[39]";
  cout << " word[40]";
  cout << " word[41]";
  cout << " word[42]";
  cout << " word[43]";
  cout << " word[44]";
  cout << " word[45]";
  cout << " word[46]";
  cout << " word[47]";
  cout << " word[48]";
  cout << " word[49]";
  cout << " word[50]";
  cout << " word[51]";
  cout << " word[52]";
  cout << " word[53]";
  cout << " word[54]";
  cout << " word[55]";
  cout << " word[56]";
  cout << " word[57]";
  cout << " word[58]";
  cout << " word[59]";
  cout << " word[60]";
  cout << " word[61]";
  cout << " word[62]";
  cout << " word[63]";
  cout << " word[64]";
  cout << " word[65]";
  cout << " word[66]";
  cout << " word[67]";
  cout << " word[68]";
  cout << " word[69]";
  cout << " word[70]";
  cout << " word[71]";
  cout << " word[72]";
  cout << " word[73]";
  cout << " word[74]";
  cout << " word[75]";
  cout << " word[76]";
  cout << " word[77]";
  cout << " word[78]";
  cout << " word[79]";
  cout << " word[80]";
  cout << " word[81]";
  cout << " word[82]";
  cout << " word[83]";
  cout << " word[84]";
  cout << " word[85]";
  cout << " word[86]";
  cout << " word[87]";
  cout << " word[88]";
  cout << " word[89]";
  cout << " word[90]";
  cout << " word[91]";
  cout << " word[92]";
  cout << " word[93]";
  cout << " word[94]";
  cout << " word[95]";
  cout << " word[96]";
  cout << " word[97]";
  cout << " word[98]";
  cout << " word[99]";
  cout << " word[100]";
  cout << " word[101]";
  cout << " word[102]";
  cout << " word[103]";
  cout << " word[104]";
  cout << " word[105]";
  cout << " word[106]";
  cout << " word[107]";
  cout << " word[108]";
  cout << " word[109]";
  cout << " word[110]";
  cout << " word[111]";
  cout << " word[112]";
  cout << " word[113]";
  cout << " word[114]";
  cout << " word[115]";
  cout << " word[116]";
  cout << " word[117]";
  cout << " word[118]";
  cout << " word[119]";
  cout << " word[120]";
  cout << " word[121]";
  cout << " word[122]";
  cout << " word[123]";
  cout << " word[124]";
  cout << " word[125]";
  cout << " word[126]";
  cout << " word[127]";
  cout << " word[128]";
  cout << " word[129]";
  cout << " word[130]";
  cout << " word[131]";
  cout << " word[132]";
  cout << " word[133]";
  cout << " word[134]";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].Nwords;
     cout << " " << setw(11) << fTableData[row].packetID;
     cout << " " << setw(11) << fTableData[row].hitformat;
     for(int i0=0; i0<135; i0++) {
        cout << " " << setw(11) << fTableData[row].word[i0];
     }

     cout << endl;
  }

}

void
dErtDcmDataWrapper::Print(Option_t* option) const
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
dErtDcmDataWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DERTDCMDATA_ST* newData = new DERTDCMDATA_ST[max_rows];
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
dErtDcmDataWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dErtDcmDataWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dErtDcmDataWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dErtDcmDataWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DERTDCMDATA_ST)) {
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
     fTableData = new DERTDCMDATA_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dErtDcmDataWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].Nwords;
        b >> fTableData[i].packetID;
        b >> fTableData[i].hitformat;
        b.ReadStaticArray(fTableData[i].word);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].Nwords;
        b << fTableData[i].packetID;
        b << fTableData[i].hitformat;
        b.WriteArray(fTableData[i].word,135);
     }
   }

}
/* Automatically generated.  Do not edit. */
