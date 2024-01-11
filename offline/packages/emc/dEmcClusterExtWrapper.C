#include "dEmcClusterExtWrapper.h"

ClassImp(dEmcClusterExtWrapper)

dEmcClusterExtWrapper::dEmcClusterExtWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DEMCCLUSTEREXT_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DEMCCLUSTEREXT_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DEMCCLUSTEREXT_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dEmcClusterExt");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dEmcClusterExtWrapper::~dEmcClusterExtWrapper()
{
  delete [] fTableData;
}

void*
dEmcClusterExtWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DEMCCLUSTEREXT_ST*
dEmcClusterExtWrapper::TableData()
{
  return fTableData;
}

DEMCCLUSTEREXT_ST&
dEmcClusterExtWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DEMCCLUSTEREXT_ST&
dEmcClusterExtWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dEmcClusterExtWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DEMCCLUSTEREXT_ST* newData = new DEMCCLUSTEREXT_ST[max_rows];
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
dEmcClusterExtWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  } else {
     fTableHeader->nok = n;
  }
}

void
dEmcClusterExtWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dEmcClusterExtWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dEmcClusterExtWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DEMCCLUSTEREXT_ST)) {
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
 	   fTableData = new DEMCCLUSTEREXT_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dEmcClusterExtWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].runno;
        b >> fTableData[i].evno;
        b >> fTableData[i].clusno;
        b >> fTableData[i].method;
        b >> fTableData[i].type;
        b >> fTableData[i].arm;
        b >> fTableData[i].sector;
        b.ReadStaticArray(fTableData[i].xyz);
        b.ReadStaticArray(fTableData[i].dxyz);
        b >> fTableData[i].e;
        b >> fTableData[i].ecorr;
        b >> fTableData[i].de;
        b >> fTableData[i].tof;
        b >> fTableData[i].ecent;
        b >> fTableData[i].tofcorr;
        b >> fTableData[i].dtof;
        b >> fTableData[i].qual;
        b >> fTableData[i].pid;
        b >> fTableData[i].prob_photon;
        b >> fTableData[i].prob_neuhad;
        b >> fTableData[i].chi2;
        b >> fTableData[i].nsh;
        b >> fTableData[i].chi2_sh;
        b >> fTableData[i].prob_photon_sh;
        b.ReadStaticArray(fTableData[i].e_sh);
        b.ReadStaticArray(fTableData[i].ecorr_sh);
        b.ReadStaticArray(fTableData[i].de_sh);
        b.ReadStaticArray((float *)fTableData[i].xyz_sh);
        b.ReadStaticArray((float *)fTableData[i].dxyz_sh);
        b >> fTableData[i].theta;
        b >> fTableData[i].phi;
        b.ReadStaticArray(fTableData[i].unitv);
        b.ReadStaticArray(fTableData[i].ind);
        b >> fTableData[i].twrhit;
        b >> fTableData[i].tofmin;
        b >> fTableData[i].etofmin;
        b >> fTableData[i].tofmincorr;
        b >> fTableData[i].tofmax;
        b >> fTableData[i].etofmax;
        b >> fTableData[i].tofmaxcorr;
        b >> fTableData[i].tofmean;
        b.ReadStaticArray(fTableData[i].disp);
        b.ReadStaticArray(fTableData[i].padisp);
        b >> fTableData[i].charged;
        b.ReadStaticArray(fTableData[i].pc3proj);
        b.ReadStaticArray(fTableData[i].partesum);
        b.ReadStaticArray(fTableData[i].twrlist);
        b.ReadStaticArray(fTableData[i].chglist);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].runno;
        b << fTableData[i].evno;
        b << fTableData[i].clusno;
        b << fTableData[i].method;
        b << fTableData[i].type;
        b << fTableData[i].arm;
        b << fTableData[i].sector;
        b.WriteArray(fTableData[i].xyz,3);
        b.WriteArray(fTableData[i].dxyz,3);
        b << fTableData[i].e;
        b << fTableData[i].ecorr;
        b << fTableData[i].de;
        b << fTableData[i].tof;
        b << fTableData[i].ecent;
        b << fTableData[i].tofcorr;
        b << fTableData[i].dtof;
        b << fTableData[i].qual;
        b << fTableData[i].pid;
        b << fTableData[i].prob_photon;
        b << fTableData[i].prob_neuhad;
        b << fTableData[i].chi2;
        b << fTableData[i].nsh;
        b << fTableData[i].chi2_sh;
        b << fTableData[i].prob_photon_sh;
        b.WriteArray(fTableData[i].e_sh,2);
        b.WriteArray(fTableData[i].ecorr_sh,2);
        b.WriteArray(fTableData[i].de_sh,2);
        b.WriteArray((float *)fTableData[i].xyz_sh,6);
        b.WriteArray((float *)fTableData[i].dxyz_sh,6);
        b << fTableData[i].theta;
        b << fTableData[i].phi;
        b.WriteArray(fTableData[i].unitv,3);
        b.WriteArray(fTableData[i].ind,2);
        b << fTableData[i].twrhit;
        b << fTableData[i].tofmin;
        b << fTableData[i].etofmin;
        b << fTableData[i].tofmincorr;
        b << fTableData[i].tofmax;
        b << fTableData[i].etofmax;
        b << fTableData[i].tofmaxcorr;
        b << fTableData[i].tofmean;
        b.WriteArray(fTableData[i].disp,2);
        b.WriteArray(fTableData[i].padisp,2);
        b << fTableData[i].charged;
        b.WriteArray(fTableData[i].pc3proj,3);
        b.WriteArray(fTableData[i].partesum,16);
        b.WriteArray(fTableData[i].twrlist,16);
        b.WriteArray(fTableData[i].chglist,16);
     }
   }

}

