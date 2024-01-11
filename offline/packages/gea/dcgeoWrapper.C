#include <dcgeoWrapper.h>

ClassImp(dcgeoWrapper)

dcgeoWrapper::dcgeoWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DCGEO_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DCGEO_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DCGEO_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dcgeo");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dcgeoWrapper::~dcgeoWrapper()
{
  delete [] fTableData;
}

void*
dcgeoWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DCGEO_ST*
dcgeoWrapper::TableData()
{
  return fTableData;
}

DCGEO_ST&
dcgeoWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DCGEO_ST&
dcgeoWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dcgeoWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DCGEO_ST* newData = new DCGEO_ST[max_rows];
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
dcgeoWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dcgeoWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dcgeoWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dcgeoWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DCGEO_ST)) {
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
 	   fTableData = new DCGEO_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dcgeoWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].ncells;
        b >> fTableData[i].ngusset;
        b >> fTableData[i].ti_switch;
        b >> fTableData[i].suppzlength;
        b >> fTableData[i].inradius;
        b >> fTableData[i].outradius;
        b >> fTableData[i].phibotw;
        b >> fTableData[i].phitopw;
        b >> fTableData[i].phitope;
        b >> fTableData[i].phibote;
        b.ReadStaticArray(fTableData[i].rplane);
        b >> fTableData[i].planethick;
        b >> fTableData[i].uvangle;
        b >> fTableData[i].winthickin;
        b >> fTableData[i].winthickout;
        b >> fTableData[i].supptiside;
        b >> fTableData[i].suppalside;
        b >> fTableData[i].suppzthick;
        b >> fTableData[i].supptibase;
        b >> fTableData[i].suppalbase;
        b >> fTableData[i].x1baserad;
        b >> fTableData[i].x2baserad;
        b >> fTableData[i].x1basez;
        b >> fTableData[i].x2basez;
        b >> fTableData[i].x1slotthick;
        b >> fTableData[i].x2slotthick;
        b >> fTableData[i].x1slotz;
        b >> fTableData[i].x2slotz;
        b >> fTableData[i].x1suppthick;
        b >> fTableData[i].x2suppthick;
        b >> fTableData[i].x1suppz;
        b >> fTableData[i].x2suppz;
        b >> fTableData[i].x1rextent;
        b >> fTableData[i].x2rextent;
        b >> fTableData[i].u1rextent;
        b >> fTableData[i].v1rextent;
        b >> fTableData[i].u2rextent;
        b >> fTableData[i].v2rextent;
        b >> fTableData[i].u1basez;
        b >> fTableData[i].v1basez;
        b >> fTableData[i].u2basez;
        b >> fTableData[i].v2basez;
        b >> fTableData[i].u1slotz;
        b >> fTableData[i].v1slotz;
        b >> fTableData[i].u2slotz;
        b >> fTableData[i].v2slotz;
        b >> fTableData[i].u1suppz;
        b >> fTableData[i].v1suppz;
        b >> fTableData[i].u2suppz;
        b >> fTableData[i].v2suppz;
        b >> fTableData[i].cfibinrad;
        b >> fTableData[i].cfiboutrad;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].ncells;
        b << fTableData[i].ngusset;
        b << fTableData[i].ti_switch;
        b << fTableData[i].suppzlength;
        b << fTableData[i].inradius;
        b << fTableData[i].outradius;
        b << fTableData[i].phibotw;
        b << fTableData[i].phitopw;
        b << fTableData[i].phitope;
        b << fTableData[i].phibote;
        b.WriteArray(fTableData[i].rplane,42);
        b << fTableData[i].planethick;
        b << fTableData[i].uvangle;
        b << fTableData[i].winthickin;
        b << fTableData[i].winthickout;
        b << fTableData[i].supptiside;
        b << fTableData[i].suppalside;
        b << fTableData[i].suppzthick;
        b << fTableData[i].supptibase;
        b << fTableData[i].suppalbase;
        b << fTableData[i].x1baserad;
        b << fTableData[i].x2baserad;
        b << fTableData[i].x1basez;
        b << fTableData[i].x2basez;
        b << fTableData[i].x1slotthick;
        b << fTableData[i].x2slotthick;
        b << fTableData[i].x1slotz;
        b << fTableData[i].x2slotz;
        b << fTableData[i].x1suppthick;
        b << fTableData[i].x2suppthick;
        b << fTableData[i].x1suppz;
        b << fTableData[i].x2suppz;
        b << fTableData[i].x1rextent;
        b << fTableData[i].x2rextent;
        b << fTableData[i].u1rextent;
        b << fTableData[i].v1rextent;
        b << fTableData[i].u2rextent;
        b << fTableData[i].v2rextent;
        b << fTableData[i].u1basez;
        b << fTableData[i].v1basez;
        b << fTableData[i].u2basez;
        b << fTableData[i].v2basez;
        b << fTableData[i].u1slotz;
        b << fTableData[i].v1slotz;
        b << fTableData[i].u2slotz;
        b << fTableData[i].v2slotz;
        b << fTableData[i].u1suppz;
        b << fTableData[i].v1suppz;
        b << fTableData[i].u2suppz;
        b << fTableData[i].v2suppz;
        b << fTableData[i].cfibinrad;
        b << fTableData[i].cfiboutrad;
     }
   }

}
/* Automatically generated.  Do not edit. */
