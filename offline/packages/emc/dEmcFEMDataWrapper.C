#include "dEmcFEMDataWrapper.h"

ClassImp(dEmcFEMDataWrapper)

dEmcFEMDataWrapper::dEmcFEMDataWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DEMCFEMDATA_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DEMCFEMDATA_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DEMCFEMDATA_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dEmcFEMData");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dEmcFEMDataWrapper::~dEmcFEMDataWrapper()
{
  delete [] fTableData;
}

void*
dEmcFEMDataWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DEMCFEMDATA_ST*
dEmcFEMDataWrapper::TableData()
{
  return fTableData;
}

DEMCFEMDATA_ST&
dEmcFEMDataWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DEMCFEMDATA_ST&
dEmcFEMDataWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dEmcFEMDataWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DEMCFEMDATA_ST* newData = new DEMCFEMDATA_ST[max_rows];
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
dEmcFEMDataWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  } else {
     fTableHeader->nok = n;
  }
}

void
dEmcFEMDataWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dEmcFEMDataWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dEmcFEMDataWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DEMCFEMDATA_ST)) {
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
 	   fTableData = new DEMCFEMDATA_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dEmcFEMDataWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].header;
        b >> fTableData[i].detid;
        b >> fTableData[i].evno;
        b >> fTableData[i].module;
        b >> fTableData[i].flag;
        b >> fTableData[i].clock;
        b >> fTableData[i].timecell;
        b >> fTableData[i].precell;
        b >> fTableData[i].postcell;
        b.ReadStaticArray(fTableData[i].data);
        b.ReadStaticArray(fTableData[i].userword);
        b >> fTableData[i].longparity;
        b >> fTableData[i].trailer;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].header;
        b << fTableData[i].detid;
        b << fTableData[i].evno;
        b << fTableData[i].module;
        b << fTableData[i].flag;
        b << fTableData[i].clock;
        b << fTableData[i].timecell;
        b << fTableData[i].precell;
        b << fTableData[i].postcell;
        b.WriteArray(fTableData[i].data,720);
        b.WriteArray(fTableData[i].userword,8);
        b << fTableData[i].longparity;
        b << fTableData[i].trailer;
     }
   }

}
/* Automatically generated.  Do not edit. */
