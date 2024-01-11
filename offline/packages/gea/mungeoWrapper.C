#include "mungeoWrapper.h"

ClassImp(mungeoWrapper)

mungeoWrapper::mungeoWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(MUNGEO_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new MUNGEO_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new MUNGEO_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("mungeo");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

mungeoWrapper::~mungeoWrapper()
{
  delete [] fTableData;
}

void*
mungeoWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

MUNGEO_ST*
mungeoWrapper::TableData()
{
  return fTableData;
}

MUNGEO_ST&
mungeoWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const MUNGEO_ST&
mungeoWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
mungeoWrapper::SetMaxRowCount(const size_t& max_rows)
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
     MUNGEO_ST* newData = new MUNGEO_ST[max_rows];
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
mungeoWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
mungeoWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
mungeoWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class mungeoWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(MUNGEO_ST)) {
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
 	   fTableData = new MUNGEO_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("mungeoWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].nr_muon_arms;
        b >> fTableData[i].nr_muid_planes;
        b >> fTableData[i].ndet_per_pl;
        b >> fTableData[i].mu_floor_flg;
        b >> fTableData[i].muid_zmin;
        b >> fTableData[i].muid_zmax;
        b >> fTableData[i].muid_ang;
        b >> fTableData[i].beam_height;
        b >> fTableData[i].mu_top_height;
        b >> fTableData[i].mu_floor_height;
        b >> fTableData[i].mu_strp_thick;
        b >> fTableData[i].mu_gas_thick;
        b >> fTableData[i].mu_plas_thick;
        b.ReadStaticArray(fTableData[i].mu_yoke_thick);
        b.ReadStaticArray(fTableData[i].mu_donut_thick1);
        b.ReadStaticArray(fTableData[i].mu_donut_thick2);
        b.ReadStaticArray(fTableData[i].rmax1_donut1);
        b.ReadStaticArray(fTableData[i].rmax1_donut2);
        b.ReadStaticArray(fTableData[i].rmax2_donut1);
        b.ReadStaticArray(fTableData[i].rmax2_donut2);
        b >> fTableData[i].mu_floor_thick;
        b.ReadStaticArray(fTableData[i].mu_abs_thick);
        b.ReadStaticArray(fTableData[i].mu_hi_abs);
        b.ReadStaticArray(fTableData[i].mu_hi_med);
        b.ReadStaticArray(fTableData[i].mu_lo_med);
        b >> fTableData[i].nmed_mu_ps;
        b >> fTableData[i].nmed_mu_gas;
        b >> fTableData[i].nmed_mu_cs;
        b >> fTableData[i].nmed_mu_sh;
        b >> fTableData[i].nmed_mu_sd;
        b >> fTableData[i].nmed_mudn;
        b >> fTableData[i].nmed_muhl;
        b >> fTableData[i].nmed_mufl;
        b >> fTableData[i].nmed_yoke;
        b >> fTableData[i].mu_hi_color;
        b >> fTableData[i].mu_lo_color;
        b >> fTableData[i].color_muid;
        b >> fTableData[i].color_hole;
        b >> fTableData[i].color_dont;
        b >> fTableData[i].color_floor;
        b >> fTableData[i].color_strd;
        b >> fTableData[i].color_yoke;
        b.ReadStaticArray(fTableData[i].rykmin1);
        b.ReadStaticArray(fTableData[i].rykmin2);
        b.ReadStaticArray(fTableData[i].rmin_donut);
        b.ReadStaticArray(fTableData[i].zyoke);
        b >> fTableData[i].zgap_yoke_abs;
        b >> fTableData[i].zgap_labs_ldet;
        b >> fTableData[i].muid_delx;
        b >> fTableData[i].str_xstag;
        b >> fTableData[i].muhl_shld_flag;
        b >> fTableData[i].color_muhl_shld;
        b >> fTableData[i].nmed_muhl_shld;
        b.ReadStaticArray(fTableData[i].z_muhl_shld);
        b >> fTableData[i].thick_muhl_shld;
        b >> fTableData[i].muhl_config_flag;
        b >> fTableData[i].muabs_config_flag;
        b >> fTableData[i].npl_muhl;
        b.ReadStaticArray(fTableData[i].z_muhl);
        b.ReadStaticArray(fTableData[i].rmin_muhl);
        b.ReadStaticArray(fTableData[i].rmax_muhl);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].nr_muon_arms;
        b << fTableData[i].nr_muid_planes;
        b << fTableData[i].ndet_per_pl;
        b << fTableData[i].mu_floor_flg;
        b << fTableData[i].muid_zmin;
        b << fTableData[i].muid_zmax;
        b << fTableData[i].muid_ang;
        b << fTableData[i].beam_height;
        b << fTableData[i].mu_top_height;
        b << fTableData[i].mu_floor_height;
        b << fTableData[i].mu_strp_thick;
        b << fTableData[i].mu_gas_thick;
        b << fTableData[i].mu_plas_thick;
        b.WriteArray(fTableData[i].mu_yoke_thick,2);
        b.WriteArray(fTableData[i].mu_donut_thick1,2);
        b.WriteArray(fTableData[i].mu_donut_thick2,2);
        b.WriteArray(fTableData[i].rmax1_donut1,2);
        b.WriteArray(fTableData[i].rmax1_donut2,2);
        b.WriteArray(fTableData[i].rmax2_donut1,2);
        b.WriteArray(fTableData[i].rmax2_donut2,2);
        b << fTableData[i].mu_floor_thick;
        b.WriteArray(fTableData[i].mu_abs_thick,11);
        b.WriteArray(fTableData[i].mu_hi_abs,11);
        b.WriteArray(fTableData[i].mu_hi_med,11);
        b.WriteArray(fTableData[i].mu_lo_med,11);
        b << fTableData[i].nmed_mu_ps;
        b << fTableData[i].nmed_mu_gas;
        b << fTableData[i].nmed_mu_cs;
        b << fTableData[i].nmed_mu_sh;
        b << fTableData[i].nmed_mu_sd;
        b << fTableData[i].nmed_mudn;
        b << fTableData[i].nmed_muhl;
        b << fTableData[i].nmed_mufl;
        b << fTableData[i].nmed_yoke;
        b << fTableData[i].mu_hi_color;
        b << fTableData[i].mu_lo_color;
        b << fTableData[i].color_muid;
        b << fTableData[i].color_hole;
        b << fTableData[i].color_dont;
        b << fTableData[i].color_floor;
        b << fTableData[i].color_strd;
        b << fTableData[i].color_yoke;
        b.WriteArray(fTableData[i].rykmin1,2);
        b.WriteArray(fTableData[i].rykmin2,2);
        b.WriteArray(fTableData[i].rmin_donut,2);
        b.WriteArray(fTableData[i].zyoke,2);
        b << fTableData[i].zgap_yoke_abs;
        b << fTableData[i].zgap_labs_ldet;
        b << fTableData[i].muid_delx;
        b << fTableData[i].str_xstag;
        b << fTableData[i].muhl_shld_flag;
        b << fTableData[i].color_muhl_shld;
        b << fTableData[i].nmed_muhl_shld;
        b.WriteArray(fTableData[i].z_muhl_shld,2);
        b << fTableData[i].thick_muhl_shld;
        b << fTableData[i].muhl_config_flag;
        b << fTableData[i].muabs_config_flag;
        b << fTableData[i].npl_muhl;
        b.WriteArray(fTableData[i].z_muhl,6);
        b.WriteArray(fTableData[i].rmin_muhl,6);
        b.WriteArray(fTableData[i].rmax_muhl,6);
     }
   }

}
/* Automatically generated.  Do not edit. */
