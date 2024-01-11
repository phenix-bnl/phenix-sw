#include "dEmcGeaClusterTrackWrapper.h"

ClassImp(dEmcGeaClusterTrackWrapper)

dEmcGeaClusterTrackWrapper::dEmcGeaClusterTrackWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DEMCGEACLUSTERTRACK_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DEMCGEACLUSTERTRACK_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DEMCGEACLUSTERTRACK_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dEmcGeaClusterTrack");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dEmcGeaClusterTrackWrapper::~dEmcGeaClusterTrackWrapper()
{
  delete [] fTableData;
}

void*
dEmcGeaClusterTrackWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DEMCGEACLUSTERTRACK_ST*
dEmcGeaClusterTrackWrapper::TableData()
{
  return fTableData;
}

DEMCGEACLUSTERTRACK_ST&
dEmcGeaClusterTrackWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DEMCGEACLUSTERTRACK_ST&
dEmcGeaClusterTrackWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dEmcGeaClusterTrackWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DEMCGEACLUSTERTRACK_ST* newData = new DEMCGEACLUSTERTRACK_ST[max_rows];
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
dEmcGeaClusterTrackWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  } else {
     fTableHeader->nok = n;
  }
}

void
dEmcGeaClusterTrackWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dEmcGeaClusterTrackWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dEmcGeaClusterTrackWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DEMCGEACLUSTERTRACK_ST)) {
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
 	   fTableData = new DEMCGEACLUSTERTRACK_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dEmcGeaClusterTrackWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].clusid;
        b >> fTableData[i].evno;
        b >> fTableData[i].keycent;
        b >> fTableData[i].input;
        b >> fTableData[i].type;
        b >> fTableData[i].arm;
        b >> fTableData[i].sector;
        b.ReadStaticArray(fTableData[i].trkno);
        b.ReadStaticArray(fTableData[i].tracktwrhit);
        b.ReadStaticArray(fTableData[i].edep_nom);
        b.ReadStaticArray(fTableData[i].pid);
        b.ReadStaticArray(fTableData[i].ptot);
        b.ReadStaticArray((float *)fTableData[i].vertex);
        b.ReadStaticArray(fTableData[i].ancestry);
        b.ReadStaticArray((float *)fTableData[i].xyz);
        b.ReadStaticArray(fTableData[i].edep);
        b.ReadStaticArray(fTableData[i].efrac);
        b.ReadStaticArray(fTableData[i].measxyz);
        b >> fTableData[i].mease;
        b >> fTableData[i].ecore;
        b >> fTableData[i].tof;
        b >> fTableData[i].etof;
        b >> fTableData[i].tofmin;
        b >> fTableData[i].etofmin;
        b >> fTableData[i].tofmax;
        b >> fTableData[i].etofmax;
        b >> fTableData[i].twrhit;
        b.ReadStaticArray(fTableData[i].disp);
        b.ReadStaticArray(fTableData[i].padisp);
        b.ReadStaticArray(fTableData[i].partesum);
        b >> fTableData[i].charged;
        b.ReadStaticArray(fTableData[i].pc3proj);
        b >> fTableData[i].chi2_sh;
        b >> fTableData[i].prob_photon_sh;
        b.ReadStaticArray(fTableData[i].e_sh);
        b.ReadStaticArray(fTableData[i].chglist);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].clusid;
        b << fTableData[i].evno;
        b << fTableData[i].keycent;
        b << fTableData[i].input;
        b << fTableData[i].type;
        b << fTableData[i].arm;
        b << fTableData[i].sector;
        b.WriteArray(fTableData[i].trkno,3);
        b.WriteArray(fTableData[i].tracktwrhit,3);
        b.WriteArray(fTableData[i].edep_nom,3);
        b.WriteArray(fTableData[i].pid,3);
        b.WriteArray(fTableData[i].ptot,3);
        b.WriteArray((float *)fTableData[i].vertex,9);
        b.WriteArray(fTableData[i].ancestry,3);
        b.WriteArray((float *)fTableData[i].xyz,9);
        b.WriteArray(fTableData[i].edep,3);
        b.WriteArray(fTableData[i].efrac,3);
        b.WriteArray(fTableData[i].measxyz,3);
        b << fTableData[i].mease;
        b << fTableData[i].ecore;
        b << fTableData[i].tof;
        b << fTableData[i].etof;
        b << fTableData[i].tofmin;
        b << fTableData[i].etofmin;
        b << fTableData[i].tofmax;
        b << fTableData[i].etofmax;
        b << fTableData[i].twrhit;
        b.WriteArray(fTableData[i].disp,2);
        b.WriteArray(fTableData[i].padisp,2);
        b.WriteArray(fTableData[i].partesum,8);
        b << fTableData[i].charged;
        b.WriteArray(fTableData[i].pc3proj,3);
        b << fTableData[i].chi2_sh;
        b << fTableData[i].prob_photon_sh;
        b.WriteArray(fTableData[i].e_sh,2);
        b.WriteArray(fTableData[i].chglist,8);
     }
   }

}

