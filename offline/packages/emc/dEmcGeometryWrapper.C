#include "dEmcGeometryWrapper.h"

ClassImp(dEmcGeometryWrapper)

dEmcGeometryWrapper::dEmcGeometryWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DEMCGEOMETRY_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DEMCGEOMETRY_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DEMCGEOMETRY_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dEmcGeometry");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dEmcGeometryWrapper::~dEmcGeometryWrapper()
{
  delete [] fTableData;
}

void*
dEmcGeometryWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DEMCGEOMETRY_ST*
dEmcGeometryWrapper::TableData()
{
  return fTableData;
}

DEMCGEOMETRY_ST&
dEmcGeometryWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DEMCGEOMETRY_ST&
dEmcGeometryWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dEmcGeometryWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DEMCGEOMETRY_ST* newData = new DEMCGEOMETRY_ST[max_rows];
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
dEmcGeometryWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  } else {
     fTableHeader->nok = n;
  }
}

void
dEmcGeometryWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dEmcGeometryWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dEmcGeometryWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DEMCGEOMETRY_ST)) {
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
 	   fTableData = new DEMCGEOMETRY_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dEmcGeometryWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].hwkey;
        b >> fTableData[i].swkey;
        b >> fTableData[i].arm;
        b >> fTableData[i].sector;
        b.ReadStaticArray(fTableData[i].ind);
        b.ReadStaticArray(fTableData[i].nomxyz);
        b.ReadStaticArray(fTableData[i].actxyz);
        b.ReadStaticArray(fTableData[i].nomunitv);
        b.ReadStaticArray(fTableData[i].actunitv);
        b >> fTableData[i].nomtheta;
        b >> fTableData[i].nomphi;
        b >> fTableData[i].acttheta;
        b >> fTableData[i].actphi;
        b >> fTableData[i].nomdist;
        b >> fTableData[i].actdist;
        b >> fTableData[i].nomflash;
        b >> fTableData[i].actflash;
        b >> fTableData[i].sectheta;
        b >> fTableData[i].secphi;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].hwkey;
        b << fTableData[i].swkey;
        b << fTableData[i].arm;
        b << fTableData[i].sector;
        b.WriteArray(fTableData[i].ind,2);
        b.WriteArray(fTableData[i].nomxyz,3);
        b.WriteArray(fTableData[i].actxyz,3);
        b.WriteArray(fTableData[i].nomunitv,3);
        b.WriteArray(fTableData[i].actunitv,3);
        b << fTableData[i].nomtheta;
        b << fTableData[i].nomphi;
        b << fTableData[i].acttheta;
        b << fTableData[i].actphi;
        b << fTableData[i].nomdist;
        b << fTableData[i].actdist;
        b << fTableData[i].nomflash;
        b << fTableData[i].actflash;
        b << fTableData[i].sectheta;
        b << fTableData[i].secphi;
     }
   }

}
/* Automatically generated.  Do not edit. */
