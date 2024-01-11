#include <cstdlib>
#include <dPadGeomWrapper.h>

ClassImp(dPadGeomWrapper)

dPadGeomWrapper::dPadGeomWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DPADGEOM_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DPADGEOM_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DPADGEOM_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dPadGeom");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dPadGeomWrapper::~dPadGeomWrapper()
{
  delete [] fTableData;
}

void*
dPadGeomWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DPADGEOM_ST*
dPadGeomWrapper::TableData()
{
  return fTableData;
}

DPADGEOM_ST&
dPadGeomWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DPADGEOM_ST&
dPadGeomWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dPadGeomWrapper::SetMaxRowCount(const size_t& max_rows)
{
  // Avoid reallocing a space of zero size!
  if ((int) max_rows <= 0) {
     return;
  }

  // Ensure that the current row count is not out of range.
  if (fTableHeader->nok > (int) max_rows) {
     fTableHeader->nok = max_rows;
  }

  // If table needs to grow, allocate a new area for it.
  if ((int) max_rows > fTableHeader->maxlen) {
     DPADGEOM_ST* newData = new DPADGEOM_ST[max_rows];
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
dPadGeomWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dPadGeomWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dPadGeomWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dPadGeomWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DPADGEOM_ST)) {
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
 	   fTableData = new DPADGEOM_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dPadGeomWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b.ReadStaticArray(fTableData[i].pdxoff);
        b.ReadStaticArray(fTableData[i].pdzoff);
        b.ReadStaticArray(fTableData[i].pdgas);
        b.ReadStaticArray(fTableData[i].aasep);
        b.ReadStaticArray(fTableData[i].pxlen);
        b.ReadStaticArray(fTableData[i].wside);
        b.ReadStaticArray(fTableData[i].wcent);
        b.ReadStaticArray(fTableData[i].pxsep);
        b.ReadStaticArray(fTableData[i].clsep);
        b.ReadStaticArray(fTableData[i].npdsec);
        b.ReadStaticArray(fTableData[i].npdwr);
        b.ReadStaticArray(fTableData[i].npdx);
        b.ReadStaticArray(fTableData[i].npdz);
        b.ReadStaticArray(fTableData[i].sectperarm);
        b.ReadStaticArray(fTableData[i].inradius);
        b.ReadStaticArray(fTableData[i].zgap);
        b >> fTableData[i].phibote;
        b >> fTableData[i].phitope;
        b >> fTableData[i].phibotw;
        b >> fTableData[i].phitopw;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b.WriteArray(fTableData[i].pdxoff,3);
        b.WriteArray(fTableData[i].pdzoff,3);
        b.WriteArray(fTableData[i].pdgas,3);
        b.WriteArray(fTableData[i].aasep,3);
        b.WriteArray(fTableData[i].pxlen,3);
        b.WriteArray(fTableData[i].wside,3);
        b.WriteArray(fTableData[i].wcent,3);
        b.WriteArray(fTableData[i].pxsep,3);
        b.WriteArray(fTableData[i].clsep,3);
        b.WriteArray(fTableData[i].npdsec,3);
        b.WriteArray(fTableData[i].npdwr,3);
        b.WriteArray(fTableData[i].npdx,3);
        b.WriteArray(fTableData[i].npdz,3);
        b.WriteArray(fTableData[i].sectperarm,3);
        b.WriteArray(fTableData[i].inradius,3);
        b.WriteArray(fTableData[i].zgap,3);
        b << fTableData[i].phibote;
        b << fTableData[i].phitope;
        b << fTableData[i].phibotw;
        b << fTableData[i].phitopw;
     }
   }

}
/* Automatically generated.  Do not edit. */
