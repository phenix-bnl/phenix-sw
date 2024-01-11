#include "dEmcRecoParWrapper.h"

ClassImp(dEmcRecoParWrapper)

dEmcRecoParWrapper::dEmcRecoParWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DEMCRECOPAR_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DEMCRECOPAR_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DEMCRECOPAR_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dEmcRecoPar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dEmcRecoParWrapper::~dEmcRecoParWrapper()
{
  delete [] fTableData;
}

void*
dEmcRecoParWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DEMCRECOPAR_ST*
dEmcRecoParWrapper::TableData()
{
  return fTableData;
}

DEMCRECOPAR_ST&
dEmcRecoParWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DEMCRECOPAR_ST&
dEmcRecoParWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dEmcRecoParWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DEMCRECOPAR_ST* newData = new DEMCRECOPAR_ST[max_rows];
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
dEmcRecoParWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  } else {
     fTableHeader->nok = n;
  }
}

void
dEmcRecoParWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dEmcRecoParWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dEmcRecoParWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DEMCRECOPAR_ST)) {
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
 	   fTableData = new DEMCRECOPAR_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dEmcRecoParWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].anyset;
        b >> fTableData[i].extended_output;
        b >> fTableData[i].maxtowers;
        b >> fTableData[i].clus_thr;
        b >> fTableData[i].clus_cut;
        b >> fTableData[i].clus_w0;
        b >> fTableData[i].TowerThresh;
        b >> fTableData[i].eClustMin;
        b >> fTableData[i].tmppar;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].anyset;
        b << fTableData[i].extended_output;
        b << fTableData[i].maxtowers;
        b << fTableData[i].clus_thr;
        b << fTableData[i].clus_cut;
        b << fTableData[i].clus_w0;
        b << fTableData[i].TowerThresh;
        b << fTableData[i].eClustMin;
        b << fTableData[i].tmppar;
     }
   }

}
/* Automatically generated.  Do not edit. */
