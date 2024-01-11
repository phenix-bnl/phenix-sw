#include <dCrkFEMWrapper.h>

ClassImp(dCrkFEMWrapper)

dCrkFEMWrapper::dCrkFEMWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DCRKFEM_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DCRKFEM_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DCRKFEM_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dCrkFEM");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dCrkFEMWrapper::~dCrkFEMWrapper()
{
  delete [] fTableData;
}

void*
dCrkFEMWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DCRKFEM_ST*
dCrkFEMWrapper::TableData()
{
  return fTableData;
}

DCRKFEM_ST&
dCrkFEMWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DCRKFEM_ST&
dCrkFEMWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dCrkFEMWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DCRKFEM_ST* newData = new DCRKFEM_ST[max_rows];
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
dCrkFEMWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dCrkFEMWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dCrkFEMWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dCrkFEMWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DCRKFEM_ST)) {
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
 	   fTableData = new DCRKFEM_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dCrkFEMWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].DAV1;
        b >> fTableData[i].detid;
        b >> fTableData[i].evno;
        b >> fTableData[i].module;
        b >> fTableData[i].flag;
        b >> fTableData[i].clock;
        b >> fTableData[i].tac_cell;
        b >> fTableData[i].pre_cell;
        b >> fTableData[i].post_cell;
        b.ReadStaticArray(fTableData[i].data);
        b.ReadStaticArray(fTableData[i].usr);
        b >> fTableData[i].parity;
        b >> fTableData[i].CAV2;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].DAV1;
        b << fTableData[i].detid;
        b << fTableData[i].evno;
        b << fTableData[i].module;
        b << fTableData[i].flag;
        b << fTableData[i].clock;
        b << fTableData[i].tac_cell;
        b << fTableData[i].pre_cell;
        b << fTableData[i].post_cell;
        b.WriteArray(fTableData[i].data,480);
        b.WriteArray(fTableData[i].usr,8);
        b << fTableData[i].parity;
        b << fTableData[i].CAV2;
     }
   }

}
/* Automatically generated.  Do not edit. */
