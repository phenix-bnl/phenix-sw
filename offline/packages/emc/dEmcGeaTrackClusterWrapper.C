#include "dEmcGeaTrackClusterWrapper.h"

ClassImp(dEmcGeaTrackClusterWrapper)

dEmcGeaTrackClusterWrapper::dEmcGeaTrackClusterWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DEMCGEATRACKCLUSTER_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DEMCGEATRACKCLUSTER_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DEMCGEATRACKCLUSTER_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dEmcGeaTrackCluster");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dEmcGeaTrackClusterWrapper::~dEmcGeaTrackClusterWrapper()
{
  delete [] fTableData;
}

void*
dEmcGeaTrackClusterWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DEMCGEATRACKCLUSTER_ST*
dEmcGeaTrackClusterWrapper::TableData()
{
  return fTableData;
}

DEMCGEATRACKCLUSTER_ST&
dEmcGeaTrackClusterWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DEMCGEATRACKCLUSTER_ST&
dEmcGeaTrackClusterWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dEmcGeaTrackClusterWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DEMCGEATRACKCLUSTER_ST* newData = new DEMCGEATRACKCLUSTER_ST[max_rows];
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
dEmcGeaTrackClusterWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  } else {
     fTableHeader->nok = n;
  }
}

void
dEmcGeaTrackClusterWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dEmcGeaTrackClusterWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dEmcGeaTrackClusterWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DEMCGEATRACKCLUSTER_ST)) {
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
 	   fTableData = new DEMCGEATRACKCLUSTER_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dEmcGeaTrackClusterWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].trkno;
        b >> fTableData[i].track_ptr;
        b >> fTableData[i].input;
        b.ReadStaticArray(fTableData[i].clusid);
        b >> fTableData[i].pid;
        b >> fTableData[i].ptot;
        b >> fTableData[i].nom_edep;
        b.ReadStaticArray(fTableData[i].edep);
        b.ReadStaticArray(fTableData[i].efrac);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].trkno;
        b << fTableData[i].track_ptr;
        b << fTableData[i].input;
        b.WriteArray(fTableData[i].clusid,3);
        b << fTableData[i].pid;
        b << fTableData[i].ptot;
        b << fTableData[i].nom_edep;
        b.WriteArray(fTableData[i].edep,3);
        b.WriteArray(fTableData[i].efrac,3);
     }
   }

}
/* Automatically generated.  Do not edit. */
