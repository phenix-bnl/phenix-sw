#include "emcparWrapper.h"

ClassImp(emcparWrapper)

emcparWrapper::emcparWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(EMCPAR_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new EMCPAR_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new EMCPAR_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("emcpar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

emcparWrapper::~emcparWrapper()
{
  delete [] fTableData;
}

void*
emcparWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

EMCPAR_ST*
emcparWrapper::TableData()
{
  return fTableData;
}

EMCPAR_ST&
emcparWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const EMCPAR_ST&
emcparWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
emcparWrapper::SetMaxRowCount(const size_t& max_rows)
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
     EMCPAR_ST* newData = new EMCPAR_ST[max_rows];
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
emcparWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
emcparWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
emcparWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class emcparWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(EMCPAR_ST)) {
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
 	   fTableData = new EMCPAR_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("emcparWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].emc_walls;
        b >> fTableData[i].emc_opt;
        b >> fTableData[i].iwall;
        b >> fTableData[i].itype;
        b >> fTableData[i].angle;
        b >> fTableData[i].rpos;
        b >> fTableData[i].zc_start;
        b >> fTableData[i].yc_start;
        b >> fTableData[i].lsiz;
        b >> fTableData[i].tsiz;
        b >> fTableData[i].no_modz;
        b >> fTableData[i].no_mody;
        b >> fTableData[i].no_smodz;
        b >> fTableData[i].no_smody;
        b >> fTableData[i].scint_emc_med;
        b >> fTableData[i].emc_debug;
        b.ReadStaticArray(fTableData[i].gcuts);
        b >> fTableData[i].emc_r_min_sc;
        b >> fTableData[i].emc_r_max_sc;
        b >> fTableData[i].emc_r_step;
        b >> fTableData[i].emc_z_min;
        b >> fTableData[i].emc_z_max;
        b >> fTableData[i].emc_z_step;
        b >> fTableData[i].emc_x_min_sc;
        b >> fTableData[i].emc_x_max_sc;
        b >> fTableData[i].emc_x_step;
        b >> fTableData[i].emc_dele_max_sc;
        b >> fTableData[i].emc_dele_step_sc;
        b >> fTableData[i].emc_tof_min;
        b >> fTableData[i].emc_tof_max;
        b >> fTableData[i].emc_tof_step;
        b >> fTableData[i].emc_ind1_max_sc;
        b >> fTableData[i].emc_ind2_max_sc;
        b >> fTableData[i].emc_iwall_max;
        b >> fTableData[i].emc_itype_max;
        b >> fTableData[i].emc_i1_max;
        b >> fTableData[i].emc_itrack_max;
        b >> fTableData[i].emc_spart_max;
        b >> fTableData[i].emc_ncycle_max;
        b >> fTableData[i].emc_cutgam;
        b >> fTableData[i].emc_cutele;
        b >> fTableData[i].emc_cutneu;
        b >> fTableData[i].emc_cuthad;
        b >> fTableData[i].emc_cutmuo;
        b.ReadStaticArray(fTableData[i].array);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].emc_walls;
        b << fTableData[i].emc_opt;
        b << fTableData[i].iwall;
        b << fTableData[i].itype;
        b << fTableData[i].angle;
        b << fTableData[i].rpos;
        b << fTableData[i].zc_start;
        b << fTableData[i].yc_start;
        b << fTableData[i].lsiz;
        b << fTableData[i].tsiz;
        b << fTableData[i].no_modz;
        b << fTableData[i].no_mody;
        b << fTableData[i].no_smodz;
        b << fTableData[i].no_smody;
        b << fTableData[i].scint_emc_med;
        b << fTableData[i].emc_debug;
        b.WriteArray(fTableData[i].gcuts,5);
        b << fTableData[i].emc_r_min_sc;
        b << fTableData[i].emc_r_max_sc;
        b << fTableData[i].emc_r_step;
        b << fTableData[i].emc_z_min;
        b << fTableData[i].emc_z_max;
        b << fTableData[i].emc_z_step;
        b << fTableData[i].emc_x_min_sc;
        b << fTableData[i].emc_x_max_sc;
        b << fTableData[i].emc_x_step;
        b << fTableData[i].emc_dele_max_sc;
        b << fTableData[i].emc_dele_step_sc;
        b << fTableData[i].emc_tof_min;
        b << fTableData[i].emc_tof_max;
        b << fTableData[i].emc_tof_step;
        b << fTableData[i].emc_ind1_max_sc;
        b << fTableData[i].emc_ind2_max_sc;
        b << fTableData[i].emc_iwall_max;
        b << fTableData[i].emc_itype_max;
        b << fTableData[i].emc_i1_max;
        b << fTableData[i].emc_itrack_max;
        b << fTableData[i].emc_spart_max;
        b << fTableData[i].emc_ncycle_max;
        b << fTableData[i].emc_cutgam;
        b << fTableData[i].emc_cutele;
        b << fTableData[i].emc_cutneu;
        b << fTableData[i].emc_cuthad;
        b << fTableData[i].emc_cutmuo;
        b.WriteArray(fTableData[i].array,32);
     }
   }

}
/* Automatically generated.  Do not edit. */
