#include "dEmcGeaHitWrapper.h"

ClassImp(dEmcGeaHitWrapper)

dEmcGeaHitWrapper::dEmcGeaHitWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DEMCGEAHIT_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DEMCGEAHIT_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DEMCGEAHIT_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dEmcGeaHit");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dEmcGeaHitWrapper::~dEmcGeaHitWrapper()
{
  delete [] fTableData;
}

void*
dEmcGeaHitWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DEMCGEAHIT_ST*
dEmcGeaHitWrapper::TableData()
{
  return fTableData;
}

DEMCGEAHIT_ST&
dEmcGeaHitWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DEMCGEAHIT_ST&
dEmcGeaHitWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dEmcGeaHitWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DEMCGEAHIT_ST* newData = new DEMCGEAHIT_ST[max_rows];
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
dEmcGeaHitWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  } else {
     fTableHeader->nok = n;
  }
}

void
dEmcGeaHitWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dEmcGeaHitWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dEmcGeaHitWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DEMCGEAHIT_ST)) {
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
 	   fTableData = new DEMCGEAHIT_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dEmcGeaHitWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].type;
        b >> fTableData[i].sector;
        b >> fTableData[i].smodind;
        b >> fTableData[i].towerind;
        b >> fTableData[i].deltae;
        b.ReadStaticArray(fTableData[i].xyz);
        b >> fTableData[i].tof;
        b >> fTableData[i].numed;
        b >> fTableData[i].partid;
        b >> fTableData[i].itrack;
        b >> fTableData[i].isubevt;
        b >> fTableData[i].nfile;
        b >> fTableData[i].true_track;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].type;
        b << fTableData[i].sector;
        b << fTableData[i].smodind;
        b << fTableData[i].towerind;
        b << fTableData[i].deltae;
        b.WriteArray(fTableData[i].xyz,3);
        b << fTableData[i].tof;
        b << fTableData[i].numed;
        b << fTableData[i].partid;
        b << fTableData[i].itrack;
        b << fTableData[i].isubevt;
        b << fTableData[i].nfile;
        b << fTableData[i].true_track;
     }
   }

}
/* Automatically generated.  Do not edit. */
