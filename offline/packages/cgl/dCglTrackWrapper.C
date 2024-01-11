#include <dCglTrackWrapper.h>

ClassImp(dCglTrackWrapper)

dCglTrackWrapper::dCglTrackWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DCGLTRACK_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DCGLTRACK_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DCGLTRACK_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dCglTrack");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dCglTrackWrapper::~dCglTrackWrapper()
{
  delete [] fTableData;
}

void*
dCglTrackWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DCGLTRACK_ST*
dCglTrackWrapper::TableData()
{
  return fTableData;
}

DCGLTRACK_ST&
dCglTrackWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DCGLTRACK_ST&
dCglTrackWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dCglTrackWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DCGLTRACK_ST* newData = new DCGLTRACK_ST[max_rows];
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
dCglTrackWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dCglTrackWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dCglTrackWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dCglTrackWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DCGLTRACK_ST)) {
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
 	   fTableData = new DCGLTRACK_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dCglTrackWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].arm;
        b >> fTableData[i].dctracksid;
        b >> fTableData[i].tectrackid;
        b >> fTableData[i].pc1clusid;
        b >> fTableData[i].pc2clusid;
        b >> fTableData[i].pc3clusid;
        b >> fTableData[i].tofrecid;
        b >> fTableData[i].emcclusid;
        b >> fTableData[i].richringid;
        b >> fTableData[i].quality;
        b >> fTableData[i].trackModel;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].arm;
        b << fTableData[i].dctracksid;
        b << fTableData[i].tectrackid;
        b << fTableData[i].pc1clusid;
        b << fTableData[i].pc2clusid;
        b << fTableData[i].pc3clusid;
        b << fTableData[i].tofrecid;
        b << fTableData[i].emcclusid;
        b << fTableData[i].richringid;
        b << fTableData[i].quality;
        b << fTableData[i].trackModel;
     }
   }

}
/* Automatically generated.  Do not edit. */
