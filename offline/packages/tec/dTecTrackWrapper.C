#include <cstdlib>
#include "dTecTrackWrapper.h"

ClassImp(dTecTrackWrapper)

dTecTrackWrapper::dTecTrackWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DTECTRACK_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DTECTRACK_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DTECTRACK_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dTecTrack");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dTecTrackWrapper::~dTecTrackWrapper()
{
  delete [] fTableData;
}

void*
dTecTrackWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DTECTRACK_ST*
dTecTrackWrapper::TableData()
{
  return fTableData;
}

DTECTRACK_ST&
dTecTrackWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DTECTRACK_ST&
dTecTrackWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dTecTrackWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DTECTRACK_ST* newData = new DTECTRACK_ST[max_rows];
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
dTecTrackWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dTecTrackWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dTecTrackWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dTecTrackWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DTECTRACK_ST)) {
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
 	   fTableData = new DTECTRACK_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dTecTrackWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b.ReadStaticArray(fTableData[i].xyzin);
        b.ReadStaticArray(fTableData[i].xyzout);
        b.ReadStaticArray(fTableData[i].dxyin);
        b.ReadStaticArray(fTableData[i].dxyout);
        b >> fTableData[i].quality;
        b >> fTableData[i].nhits;
        b >> fTableData[i].ntime;
        b >> fTableData[i].pid;
        b >> fTableData[i].pidqual;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b.WriteArray(fTableData[i].xyzin,3);
        b.WriteArray(fTableData[i].xyzout,3);
        b.WriteArray(fTableData[i].dxyin,2);
        b.WriteArray(fTableData[i].dxyout,2);
        b << fTableData[i].quality;
        b << fTableData[i].nhits;
        b << fTableData[i].ntime;
        b << fTableData[i].pid;
        b << fTableData[i].pidqual;
     }
   }

}

int dTecTrackWrapper::get_NFwires(size_t n){

  int nn = fTableData[n].nhits;

  int n6 = nn/100000;
  int n5 = (nn - n6*100000)/10000;
  int n4 = (nn - n6*100000 - n5*10000)/1000;
  int n3 = (nn - n6*100000 - n5*10000 - n4*1000)/100;
  int n2 = (nn - n6*100000 - n5*10000 - n4*1000 - n3*100)/10;
  int n1 = (nn - n6*100000 - n5*10000 - n4*1000 - n3*100 - n2*10);

    int nfwires=0;
    nfwires+=n1;
    nfwires+=n2;
    nfwires+=n3;
    nfwires+=n4;
    nfwires+=n5;
    nfwires+=n6;

      return nfwires;
}

int dTecTrackWrapper::get_NFplanes(size_t n){

  int nn = fTableData[n].nhits;

  int n6 = nn/100000;
  int n5 = (nn - n6*100000)/10000;
  int n4 = (nn - n6*100000 - n5*10000)/1000;
  int n3 = (nn - n6*100000 - n5*10000 - n4*1000)/100;
  int n2 = (nn - n6*100000 - n5*10000 - n4*1000 - n3*100)/10;
  int n1 = (nn - n6*100000 - n5*10000 - n4*1000 - n3*100 - n2*10);

    int nfplanes=0;
    if(n1>0) nfplanes++;
    if(n2>0) nfplanes++;
    if(n3>0) nfplanes++;
    if(n4>0) nfplanes++;
    if(n5>0) nfplanes++;
    if(n6>0) nfplanes++;

      return nfplanes;
}




