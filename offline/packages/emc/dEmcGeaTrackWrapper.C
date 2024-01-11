#include "dEmcGeaTrackWrapper.h"

ClassImp(dEmcGeaTrackWrapper)

dEmcGeaTrackWrapper::dEmcGeaTrackWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DEMCGEATRACK_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DEMCGEATRACK_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DEMCGEATRACK_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dEmcGeaTrack");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dEmcGeaTrackWrapper::~dEmcGeaTrackWrapper()
{
  delete [] fTableData;
}

void*
dEmcGeaTrackWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DEMCGEATRACK_ST*
dEmcGeaTrackWrapper::TableData()
{
  return fTableData;
}

DEMCGEATRACK_ST&
dEmcGeaTrackWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DEMCGEATRACK_ST&
dEmcGeaTrackWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dEmcGeaTrackWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DEMCGEATRACK_ST* newData = new DEMCGEATRACK_ST[max_rows];
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
dEmcGeaTrackWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  } else {
     fTableHeader->nok = n;
  }
}

void
dEmcGeaTrackWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dEmcGeaTrackWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dEmcGeaTrackWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DEMCGEATRACK_ST)) {
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
 	   fTableData = new DEMCGEATRACK_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dEmcGeaTrackWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].trkno;
        b >> fTableData[i].input;
        b >> fTableData[i].anclvl;
        b >> fTableData[i].pid;
        b >> fTableData[i].ekin;
        b.ReadStaticArray(fTableData[i].xyz);
        b >> fTableData[i].ptot;
        b.ReadStaticArray(fTableData[i].pxyz);
        b.ReadStaticArray(fTableData[i].impxyz);
        b >> fTableData[i].itparent;
        b >> fTableData[i].idparent;
        b >> fTableData[i].parent_ptr;
        b >> fTableData[i].twrhit;
        b >> fTableData[i].edep;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].trkno;
        b << fTableData[i].input;
        b << fTableData[i].anclvl;
        b << fTableData[i].pid;
        b << fTableData[i].ekin;
        b.WriteArray(fTableData[i].xyz,3);
        b << fTableData[i].ptot;
        b.WriteArray(fTableData[i].pxyz,3);
        b.WriteArray(fTableData[i].impxyz,3);
        b << fTableData[i].itparent;
        b << fTableData[i].idparent;
        b << fTableData[i].parent_ptr;
        b << fTableData[i].twrhit;
        b << fTableData[i].edep;
     }
   }

}
/* Automatically generated.  Do not edit. */
