#include <dCrkGeoWrapper.h>

ClassImp(dCrkGeoWrapper)

dCrkGeoWrapper::dCrkGeoWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DCRKGEO_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DCRKGEO_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DCRKGEO_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dCrkGeo");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dCrkGeoWrapper::~dCrkGeoWrapper()
{
  delete [] fTableData;
}

void*
dCrkGeoWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DCRKGEO_ST*
dCrkGeoWrapper::TableData()
{
  return fTableData;
}

DCRKGEO_ST&
dCrkGeoWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DCRKGEO_ST&
dCrkGeoWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dCrkGeoWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DCRKGEO_ST* newData = new DCRKGEO_ST[max_rows];
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
dCrkGeoWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dCrkGeoWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dCrkGeoWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dCrkGeoWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DCRKGEO_ST)) {
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
 	   fTableData = new DCRKGEO_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dCrkGeoWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].phi_cntr;
        b >> fTableData[i].phi_open;
        b >> fTableData[i].dphi_carm;
        b >> fTableData[i].dphi_cshe;
        b >> fTableData[i].pmt_phi_min;
        b >> fTableData[i].pmt_phi_max;
        b >> fTableData[i].pmt_dphi;
        b >> fTableData[i].r_pmt_ent;
        b.ReadStaticArray(fTableData[i].dx_pmt);
        b.ReadStaticArray(fTableData[i].r_pmt);
        b.ReadStaticArray(fTableData[i].z_pmt);
        b.ReadStaticArray(fTableData[i].theta_pmt);
        b >> fTableData[i].mir_rin;
        b >> fTableData[i].mir_thck;
        b >> fTableData[i].mir_theta1;
        b >> fTableData[i].mir_theta2;
        b >> fTableData[i].mir_thetacut;
        b >> fTableData[i].mir_phi1;
        b >> fTableData[i].mir_phi2;
        b >> fTableData[i].mir_dz;
        b >> fTableData[i].wi1_rin;
        b >> fTableData[i].wi1_thck;
        b >> fTableData[i].wi1_zend;
        b >> fTableData[i].wi2_rin;
        b >> fTableData[i].wi2_thck;
        b >> fTableData[i].wi2_zend;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].phi_cntr;
        b << fTableData[i].phi_open;
        b << fTableData[i].dphi_carm;
        b << fTableData[i].dphi_cshe;
        b << fTableData[i].pmt_phi_min;
        b << fTableData[i].pmt_phi_max;
        b << fTableData[i].pmt_dphi;
        b << fTableData[i].r_pmt_ent;
        b.WriteArray(fTableData[i].dx_pmt,32);
        b.WriteArray(fTableData[i].r_pmt,32);
        b.WriteArray(fTableData[i].z_pmt,32);
        b.WriteArray(fTableData[i].theta_pmt,32);
        b << fTableData[i].mir_rin;
        b << fTableData[i].mir_thck;
        b << fTableData[i].mir_theta1;
        b << fTableData[i].mir_theta2;
        b << fTableData[i].mir_thetacut;
        b << fTableData[i].mir_phi1;
        b << fTableData[i].mir_phi2;
        b << fTableData[i].mir_dz;
        b << fTableData[i].wi1_rin;
        b << fTableData[i].wi1_thck;
        b << fTableData[i].wi1_zend;
        b << fTableData[i].wi2_rin;
        b << fTableData[i].wi2_thck;
        b << fTableData[i].wi2_zend;
     }
   }

}
/* Automatically generated.  Do not edit. */
