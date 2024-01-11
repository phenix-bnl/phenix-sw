#include <cstring>
#include <iostream>
#include <iomanip>
#include "dBbcUcalWrapper.h"

using namespace std;

ClassImp(dBbcUcalWrapper)

dBbcUcalWrapper::dBbcUcalWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DBBCUCAL_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DBBCUCAL_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DBBCUCAL_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dBbcUcal");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dBbcUcalWrapper::dBbcUcalWrapper(const dBbcUcalWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DBBCUCAL_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DBBCUCAL_ST));
  SetType("dBbcUcal");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dBbcUcalWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dBbcUcalWrapper&
dBbcUcalWrapper::operator=(const dBbcUcalWrapper& source)
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

dBbcUcalWrapper::~dBbcUcalWrapper()
{
  delete [] fTableData;
}

DBBCUCAL_ST*
dBbcUcalWrapper::TableData()
{
  return fTableData;
}

DBBCUCAL_ST&
dBbcUcalWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DBBCUCAL_ST&
dBbcUcalWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dBbcUcalWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "Pmt";
  cout << " " << setw(11) << "AdcChGain";
  cout << " " << setw(11) << "TdcChGain0";
  cout << " " << setw(11) << "TdcChGain1";
  cout << " " << setw(11) << "TdcOffset0";
  cout << " " << setw(11) << "TdcOffset1";
  cout << " " << setw(11) << "TdcOver0_mean";
  cout << " " << setw(11) << "TdcOver0_sigma";
  cout << " " << setw(11) << "TdcOver1_mean";
  cout << " " << setw(11) << "TdcOver1_sigma";
  cout << " " << setw(11) << "TdcThreshold0";
  cout << " " << setw(11) << "TdcThreshold1";
  cout << " " << setw(11) << "PulseHeightReso";
  cout << " " << setw(11) << "PMTGain";
  cout << " " << setw(11) << "Pedestal";
  cout << " " << setw(11) << "AdcGainFac";
  cout << " " << setw(11) << "SlewParA";
  cout << " " << setw(11) << "SlewParA0";
  cout << " " << setw(11) << "SlewParB0";
  cout << " " << setw(11) << "SlewParC0";
  cout << " " << setw(11) << "SlewParA1";
  cout << " " << setw(11) << "SlewParB1";
  cout << " " << setw(11) << "SlewParC1";
  cout << " " << setw(11) << "Z0overC_off";
  cout << " " << setw(11) << "dif_off";
  cout << " " << setw(11) << "MeanTDC_off";
  cout << " " << setw(11) << "ThresholdFactor";
  cout << " " << setw(11) << "NoiseHeight";
  cout << " " << setw(11) << "NoiseHitProb";
  cout << " " << setw(11) << "TimeReso";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].Pmt;
     cout << " " << setw(11) << fTableData[row].AdcChGain;
     cout << " " << setw(11) << fTableData[row].TdcChGain0;
     cout << " " << setw(11) << fTableData[row].TdcChGain1;
     cout << " " << setw(11) << fTableData[row].TdcOffset0;
     cout << " " << setw(11) << fTableData[row].TdcOffset1;
     cout << " " << setw(11) << fTableData[row].TdcOver0_mean;
     cout << " " << setw(11) << fTableData[row].TdcOver0_sigma;
     cout << " " << setw(11) << fTableData[row].TdcOver1_mean;
     cout << " " << setw(11) << fTableData[row].TdcOver1_mean;
     cout << " " << setw(11) << fTableData[row].TdcThreshold0;
     cout << " " << setw(11) << fTableData[row].TdcThreshold1;
     cout << " " << setw(11) << fTableData[row].PulseHeightReso;
     cout << " " << setw(11) << fTableData[row].PMTGain;
     cout << " " << setw(11) << fTableData[row].Pedestal;
     cout << " " << setw(11) << fTableData[row].AdcGainFac;
     cout << " " << setw(11) << fTableData[row].SlewParA;
     cout << " " << setw(11) << fTableData[row].SlewParA0;
     cout << " " << setw(11) << fTableData[row].SlewParB0;
     cout << " " << setw(11) << fTableData[row].SlewParC0;
     cout << " " << setw(11) << fTableData[row].SlewParA1;
     cout << " " << setw(11) << fTableData[row].SlewParB1;
     cout << " " << setw(11) << fTableData[row].SlewParC1;
     cout << " " << setw(11) << fTableData[row].Z0overC_off;
     cout << " " << setw(11) << fTableData[row].dif_off;
     cout << " " << setw(11) << fTableData[row].MeanTDC_off;
     cout << " " << setw(11) << fTableData[row].ThresholdFactor;
     cout << " " << setw(11) << fTableData[row].NoiseHeight;
     cout << " " << setw(11) << fTableData[row].NoiseHitProb;
     cout << " " << setw(11) << fTableData[row].TimeReso;

     cout << endl;
  }

}

void
dBbcUcalWrapper::Print(Option_t* option) const
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
dBbcUcalWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DBBCUCAL_ST* newData = new DBBCUCAL_ST[max_rows];
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
dBbcUcalWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dBbcUcalWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dBbcUcalWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dBbcUcalWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DBBCUCAL_ST)) {
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
     fTableData = new DBBCUCAL_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dBbcUcalWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].Pmt;
        b >> fTableData[i].AdcChGain;
        b >> fTableData[i].TdcChGain0;
        b >> fTableData[i].TdcChGain1;
        b >> fTableData[i].TdcOffset0;
        b >> fTableData[i].TdcOffset1;
        b >> fTableData[i].TdcOver0_mean;
        b >> fTableData[i].TdcOver0_sigma;
        b >> fTableData[i].TdcOver1_mean;
        b >> fTableData[i].TdcOver1_sigma;
        b >> fTableData[i].TdcThreshold0;
        b >> fTableData[i].TdcThreshold1;
        b >> fTableData[i].PulseHeightReso;
        b >> fTableData[i].PMTGain;
        b >> fTableData[i].Pedestal;
        b >> fTableData[i].AdcGainFac;
        b >> fTableData[i].SlewParA;
        b >> fTableData[i].SlewParA0;
        b >> fTableData[i].SlewParB0;
        b >> fTableData[i].SlewParC0;
        b >> fTableData[i].SlewParA1;
        b >> fTableData[i].SlewParB1;
        b >> fTableData[i].SlewParC1;
        b >> fTableData[i].Z0overC_off;
        b >> fTableData[i].dif_off;
        b >> fTableData[i].MeanTDC_off;
        b >> fTableData[i].ThresholdFactor;
        b >> fTableData[i].NoiseHeight;
        b >> fTableData[i].NoiseHitProb;
        b >> fTableData[i].TimeReso;
        b >> fTableData[i].FakePede_mean;
        b >> fTableData[i].FakePede_sigma;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].Pmt;
        b << fTableData[i].AdcChGain;
        b << fTableData[i].TdcChGain0;
        b << fTableData[i].TdcChGain1;
        b << fTableData[i].TdcOffset0;
        b << fTableData[i].TdcOffset1;
        b << fTableData[i].TdcOver0_mean;
        b << fTableData[i].TdcOver0_sigma;
        b << fTableData[i].TdcOver1_mean;
        b << fTableData[i].TdcOver1_sigma;
        b << fTableData[i].TdcThreshold0;
        b << fTableData[i].TdcThreshold1;
        b << fTableData[i].PulseHeightReso;
        b << fTableData[i].PMTGain;
        b << fTableData[i].Pedestal;
        b << fTableData[i].AdcGainFac;
        b << fTableData[i].SlewParA;
        b << fTableData[i].SlewParA0;
        b << fTableData[i].SlewParB0;
        b << fTableData[i].SlewParC0;
        b << fTableData[i].SlewParA1;
        b << fTableData[i].SlewParB1;
        b << fTableData[i].SlewParC1;
        b << fTableData[i].Z0overC_off;
        b << fTableData[i].dif_off;
        b << fTableData[i].MeanTDC_off;
        b << fTableData[i].ThresholdFactor;
        b << fTableData[i].NoiseHeight;
        b << fTableData[i].NoiseHitProb;
        b << fTableData[i].TimeReso;
        b << fTableData[i].FakePede_mean;
        b << fTableData[i].FakePede_sigma;
     }
   }

}
/* Automatically generated.  Do not edit. */
