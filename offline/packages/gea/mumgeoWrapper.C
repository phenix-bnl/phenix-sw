#include "mumgeoWrapper.h"

ClassImp(mumgeoWrapper)

mumgeoWrapper::mumgeoWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(MUMGEO_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new MUMGEO_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new MUMGEO_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("mumgeo");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

mumgeoWrapper::~mumgeoWrapper()
{
  delete [] fTableData;
}

void*
mumgeoWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

MUMGEO_ST*
mumgeoWrapper::TableData()
{
  return fTableData;
}

MUMGEO_ST&
mumgeoWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const MUMGEO_ST&
mumgeoWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
mumgeoWrapper::SetMaxRowCount(const size_t& max_rows)
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
     MUMGEO_ST* newData = new MUMGEO_ST[max_rows];
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
mumgeoWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
mumgeoWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
mumgeoWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class mumgeoWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(MUMGEO_ST)) {
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
 	   fTableData = new MUMGEO_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("mumgeoWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b.ReadStaticArray(fTableData[i].mt_frame_side_thick1);
        b.ReadStaticArray(fTableData[i].mt_frame_side_thick2);
        b.ReadStaticArray(fTableData[i].mt_frame_end_thick1);
        b.ReadStaticArray(fTableData[i].mt_frame_end_thick2);
        b.ReadStaticArray(fTableData[i].mt_plane_thickness1);
        b.ReadStaticArray(fTableData[i].mt_plane_thickness2);
        b.ReadStaticArray(fTableData[i].mt_plane_spacing11);
        b.ReadStaticArray(fTableData[i].mt_plane_spacing12);
        b.ReadStaticArray(fTableData[i].mt_plane_spacing13);
        b.ReadStaticArray(fTableData[i].mt_plane_spacing14);
        b.ReadStaticArray(fTableData[i].mt_plane_spacing15);
        b.ReadStaticArray(fTableData[i].mt_plane_spacing21);
        b.ReadStaticArray(fTableData[i].mt_plane_spacing22);
        b.ReadStaticArray(fTableData[i].mt_plane_spacing23);
        b.ReadStaticArray(fTableData[i].mt_plane_spacing24);
        b.ReadStaticArray(fTableData[i].mt_plane_spacing25);
        b.ReadStaticArray(fTableData[i].mt_station_z1);
        b.ReadStaticArray(fTableData[i].mt_station_z2);
        b.ReadStaticArray(fTableData[i].mt_station_inner_radius1);
        b.ReadStaticArray(fTableData[i].mt_station_inner_radius2);
        b.ReadStaticArray(fTableData[i].mt_station_outer_radius1);
        b.ReadStaticArray(fTableData[i].mt_station_outer_radius2);
        b.ReadStaticArray(fTableData[i].mt_chm_thick1);
        b.ReadStaticArray(fTableData[i].mt_chm_thick2);
        b >> fTableData[i].mum_arms;
        b >> fTableData[i].mum_stations;
        b >> fTableData[i].mum_channels;
        b >> fTableData[i].mum_color;
        b >> fTableData[i].mu_arm_medium;
        b.ReadStaticArray(fTableData[i].mt_frame_medium1);
        b.ReadStaticArray(fTableData[i].mt_frame_medium2);
        b.ReadStaticArray(fTableData[i].mt_planes_per_station1);
        b.ReadStaticArray(fTableData[i].mt_planes_per_station2);
        b.ReadStaticArray(fTableData[i].mt_station_medium1);
        b.ReadStaticArray(fTableData[i].mt_station_medium2);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b.WriteArray(fTableData[i].mt_frame_side_thick1,5);
        b.WriteArray(fTableData[i].mt_frame_side_thick2,5);
        b.WriteArray(fTableData[i].mt_frame_end_thick1,5);
        b.WriteArray(fTableData[i].mt_frame_end_thick2,5);
        b.WriteArray(fTableData[i].mt_plane_thickness1,5);
        b.WriteArray(fTableData[i].mt_plane_thickness2,5);
        b.WriteArray(fTableData[i].mt_plane_spacing11,10);
        b.WriteArray(fTableData[i].mt_plane_spacing12,10);
        b.WriteArray(fTableData[i].mt_plane_spacing13,10);
        b.WriteArray(fTableData[i].mt_plane_spacing14,10);
        b.WriteArray(fTableData[i].mt_plane_spacing15,10);
        b.WriteArray(fTableData[i].mt_plane_spacing21,10);
        b.WriteArray(fTableData[i].mt_plane_spacing22,10);
        b.WriteArray(fTableData[i].mt_plane_spacing23,10);
        b.WriteArray(fTableData[i].mt_plane_spacing24,10);
        b.WriteArray(fTableData[i].mt_plane_spacing25,10);
        b.WriteArray(fTableData[i].mt_station_z1,5);
        b.WriteArray(fTableData[i].mt_station_z2,5);
        b.WriteArray(fTableData[i].mt_station_inner_radius1,5);
        b.WriteArray(fTableData[i].mt_station_inner_radius2,5);
        b.WriteArray(fTableData[i].mt_station_outer_radius1,5);
        b.WriteArray(fTableData[i].mt_station_outer_radius2,5);
        b.WriteArray(fTableData[i].mt_chm_thick1,5);
        b.WriteArray(fTableData[i].mt_chm_thick2,5);
        b << fTableData[i].mum_arms;
        b << fTableData[i].mum_stations;
        b << fTableData[i].mum_channels;
        b << fTableData[i].mum_color;
        b << fTableData[i].mu_arm_medium;
        b.WriteArray(fTableData[i].mt_frame_medium1,5);
        b.WriteArray(fTableData[i].mt_frame_medium2,5);
        b.WriteArray(fTableData[i].mt_planes_per_station1,5);
        b.WriteArray(fTableData[i].mt_planes_per_station2,5);
        b.WriteArray(fTableData[i].mt_station_medium1,5);
        b.WriteArray(fTableData[i].mt_station_medium2,5);
     }
   }

}
/* Automatically generated.  Do not edit. */
