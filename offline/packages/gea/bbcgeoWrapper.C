#include <bbcgeoWrapper.h>

ClassImp(bbcgeoWrapper)

bbcgeoWrapper::bbcgeoWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(BBCGEO_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new BBCGEO_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new BBCGEO_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("bbcgeo");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

bbcgeoWrapper::~bbcgeoWrapper()
{
  delete [] fTableData;
}

void*
bbcgeoWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

BBCGEO_ST*
bbcgeoWrapper::TableData()
{
  return fTableData;
}

BBCGEO_ST&
bbcgeoWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const BBCGEO_ST&
bbcgeoWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
bbcgeoWrapper::SetMaxRowCount(const size_t& max_rows)
{
  // Avoid reallocing a space of zero size!
  if (max_rows <= 0) {
     return;
  }

  // Ensure that the current row count is not out of range.
  if (fTableHeader->nok > (int) max_rows) {
     fTableHeader->nok = max_rows;
  }

  // If table needs to grow, allocate a new area for it.
  if ((int) (max_rows) > fTableHeader->maxlen) {
     BBCGEO_ST* newData = new BBCGEO_ST[max_rows];
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
bbcgeoWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
bbcgeoWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
bbcgeoWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class bbcgeoWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(BBCGEO_ST)) {
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
 	   fTableData = new BBCGEO_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("bbcgeoWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b.ReadStaticArray(fTableData[i].absorb);
        b.ReadStaticArray(fTableData[i].attach);
        b.ReadStaticArray(fTableData[i].backbd);
        b >> fTableData[i].covert;
        b.ReadStaticArray(fTableData[i].frontb);
        b.ReadStaticArray(fTableData[i].pmtsiz);
        b.ReadStaticArray(fTableData[i].quartz);
        b >> fTableData[i].spacin;
        b.ReadStaticArray(fTableData[i].struc);
        b.ReadStaticArray(fTableData[i].zposit);
        b >> fTableData[i].color;
        b >> fTableData[i].seen;
        b >> fTableData[i].medabs;
        b >> fTableData[i].medatt;
        b >> fTableData[i].medbac;
        b >> fTableData[i].medcov;
        b >> fTableData[i].medfro;
        b >> fTableData[i].medmot;
        b >> fTableData[i].medpmt;
        b >> fTableData[i].medqua;
        b >> fTableData[i].medstr;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b.WriteArray(fTableData[i].absorb,3);
        b.WriteArray(fTableData[i].attach,10);
        b.WriteArray(fTableData[i].backbd,3);
        b << fTableData[i].covert;
        b.WriteArray(fTableData[i].frontb,3);
        b.WriteArray(fTableData[i].pmtsiz,3);
        b.WriteArray(fTableData[i].quartz,10);
        b << fTableData[i].spacin;
        b.WriteArray(fTableData[i].struc,3);
        b.WriteArray(fTableData[i].zposit,2);
        b << fTableData[i].color;
        b << fTableData[i].seen;
        b << fTableData[i].medabs;
        b << fTableData[i].medatt;
        b << fTableData[i].medbac;
        b << fTableData[i].medcov;
        b << fTableData[i].medfro;
        b << fTableData[i].medmot;
        b << fTableData[i].medpmt;
        b << fTableData[i].medqua;
        b << fTableData[i].medstr;
     }
   }

}
/* Automatically generated.  Do not edit. */
