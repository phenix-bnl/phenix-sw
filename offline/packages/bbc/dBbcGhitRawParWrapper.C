#include <cstring>
#include <iostream>
#include <iomanip>
#include "dBbcGhitRawParWrapper.h"

using namespace std;

ClassImp(dBbcGhitRawParWrapper)

dBbcGhitRawParWrapper::dBbcGhitRawParWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DBBCGHITRAWPAR_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DBBCGHITRAWPAR_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DBBCGHITRAWPAR_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dBbcGhitRawPar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dBbcGhitRawParWrapper::dBbcGhitRawParWrapper(const dBbcGhitRawParWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DBBCGHITRAWPAR_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DBBCGHITRAWPAR_ST));
  SetType("dBbcGhitRawPar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dBbcGhitRawParWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dBbcGhitRawParWrapper&
dBbcGhitRawParWrapper::operator=(const dBbcGhitRawParWrapper& source)
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

dBbcGhitRawParWrapper::~dBbcGhitRawParWrapper()
{
  delete [] fTableData;
}

DBBCGHITRAWPAR_ST*
dBbcGhitRawParWrapper::TableData()
{
  return fTableData;
}

DBBCGHITRAWPAR_ST&
dBbcGhitRawParWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DBBCGHITRAWPAR_ST&
dBbcGhitRawParWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dBbcGhitRawParWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "RunNumber";
  cout << " " << setw(11) << "SimFlag";
  cout << " " << setw(11) << "randseed";
  cout << " " << setw(11) << "AngleCut";
  cout << " " << setw(11) << "Nindex";
  cout << " " << setw(11) << "N0";
  cout << " " << setw(11) << "MomLowerLim";
  cout << " " << setw(11) << "MaxAdc";
  cout << " " << setw(11) << "MaxTdc";
  cout << " " << setw(11) << "MeanTDC_off";
  cout << " " << setw(11) << "Z0overC_off";
  cout << " " << setw(11) << "RunByRun_off";
  cout << " " << setw(11) << "ThresholdFactor";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";
     cout << " " << setw(11) << fTableData[row].RunNumber;
     cout << " " << setw(11) << fTableData[row].SimFlag;
     cout << " " << setw(11) << fTableData[row].randseed;
     cout << " " << setw(11) << fTableData[row].AngleCut;
     cout << " " << setw(11) << fTableData[row].Nindex;
     cout << " " << setw(11) << fTableData[row].N0;
     cout << " " << setw(11) << fTableData[row].MomLowerLim;
     cout << " " << setw(11) << fTableData[row].MaxAdc;
     cout << " " << setw(11) << fTableData[row].MaxTdc;
     cout << " " << setw(11) << fTableData[row].MeanTDC_off;
     cout << " " << setw(11) << fTableData[row].Z0overC_off;
     cout << " " << setw(11) << fTableData[row].RunByRun_off;
     cout << " " << setw(11) << fTableData[row].ThresholdFactor;
     cout << endl;
  }

}

void
dBbcGhitRawParWrapper::Print(Option_t* option) const
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
dBbcGhitRawParWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DBBCGHITRAWPAR_ST* newData = new DBBCGHITRAWPAR_ST[max_rows];
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
dBbcGhitRawParWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dBbcGhitRawParWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dBbcGhitRawParWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dBbcGhitRawParWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DBBCGHITRAWPAR_ST)) {
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
     fTableData = new DBBCGHITRAWPAR_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dBbcGhitRawParWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].RunNumber;
        b >> fTableData[i].SimFlag;
        b >> fTableData[i].randseed;
        b >> fTableData[i].AngleCut;
        b >> fTableData[i].Nindex;
        b >> fTableData[i].N0;
        b >> fTableData[i].MomLowerLim;
        b >> fTableData[i].MaxAdc;
        b >> fTableData[i].MaxTdc;
        b >> fTableData[i].Z0overC_off;
        b >> fTableData[i].MeanTDC_off;
        b >> fTableData[i].RunByRun_off;
        b >> fTableData[i].ThresholdFactor;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].RunNumber;
        b << fTableData[i].SimFlag;
        b << fTableData[i].randseed;
        b << fTableData[i].AngleCut;
        b << fTableData[i].Nindex;
        b << fTableData[i].N0;
        b << fTableData[i].MomLowerLim;
        b << fTableData[i].MaxAdc;
        b << fTableData[i].MaxTdc;
        b << fTableData[i].Z0overC_off;
        b << fTableData[i].MeanTDC_off;
        b << fTableData[i].RunByRun_off;
        b << fTableData[i].ThresholdFactor;
     }
   }

}
/* Automatically generated.  Do not edit. */
