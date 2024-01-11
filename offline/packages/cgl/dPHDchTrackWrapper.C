#include <cstring>
#include <iostream>
#include <iomanip>
#include <dPHDchTrackWrapper.h>

using namespace std;

ClassImp(dPHDchTrackWrapper)

using namespace std;

dPHDchTrackWrapper::dPHDchTrackWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DPHDCHTRACK_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DPHDCHTRACK_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DPHDCHTRACK_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dPHDchTrack");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dPHDchTrackWrapper::dPHDchTrackWrapper(const dPHDchTrackWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DPHDCHTRACK_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DPHDCHTRACK_ST));
  SetType("dPHDchTrack");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dPHDchTrackWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dPHDchTrackWrapper&
dPHDchTrackWrapper::operator=(const dPHDchTrackWrapper& source)
{
  if (this != &source) {
     // The row count will be set by the PHTable assignment operator.

     PHTable::operator=(source);
     
     // Just copy the data from the source table.
     for (size_t row = 0; row < RowCount(); row++) {
        fTableData[row] = source.fTableData[row];
     }

  }

  return *this;
}

dPHDchTrackWrapper::~dPHDchTrackWrapper()
{
  delete [] fTableData;
}

DPHDCHTRACK_ST*
dPHDchTrackWrapper::TableData()
{
  return fTableData;
}

DPHDCHTRACK_ST&
dPHDchTrackWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DPHDCHTRACK_ST&
dPHDchTrackWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dPHDchTrackWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "id";
  cout << " " << setw(11) << "arm";
  cout << " " << setw(11) << "side";
  cout << " " << setw(11) << "charge";
  cout << " " << setw(11) << "numberOfX1X2hitsFitted";
  cout << " " << setw(11) << "numberOfSuccessfulIterations";
  cout << " " << setw(11) << "chi2";
  cout << " " << setw(11) << "ErrorCode";
  cout << " " << setw(11) << "alpha";
  cout << " " << setw(11) << "zed";
  cout << " " << setw(11) << "momentum";
  cout << " " << setw(11) << "fittedAlpha";
  cout << " " << setw(11) << "fittedPhi";
  cout << " " << setw(11) << "fittedPhi0";
  cout << " " << setw(11) << "fittedBeta";
  cout << " " << setw(11) << "fittedTheta0";
  cout << " vertex[0]";
  cout << " vertex[1]";
  cout << " vertex[2]";
  cout << " projectToVertex[0]";
  cout << " projectToVertex[1]";
  cout << " projectToVertex[2]";
  cout << " predictMomentum[0]";
  cout << " predictMomentum[1]";
  cout << " predictMomentum[2]";
  cout << " projectToPc1[0]";
  cout << " projectToPc1[1]";
  cout << " projectToPc1[2]";
  cout << " projectToPc2[0]";
  cout << " projectToPc2[1]";
  cout << " projectToPc2[2]";
  cout << " projectToPc3[0]";
  cout << " projectToPc3[1]";
  cout << " projectToPc3[2]";
  cout << " projectToTec[0]";
  cout << " projectToTec[1]";
  cout << " projectToTec[2]";
  cout << " projectToPbSc[0]";
  cout << " projectToPbSc[1]";
  cout << " projectToPbSc[2]";
  cout << " projectToPbGl[0]";
  cout << " projectToPbGl[1]";
  cout << " projectToPbGl[2]";
  cout << " projectToCrk[0]";
  cout << " projectToCrk[1]";
  cout << " projectToCrk[2]";
  cout << " projectToTof[0]";
  cout << " projectToTof[1]";
  cout << " projectToTof[2]";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].id;
     cout << " " << setw(11) << fTableData[row].arm;
     cout << " " << setw(11) << fTableData[row].side;
     cout << " " << setw(11) << fTableData[row].charge;
     cout << " " << setw(11) << fTableData[row].numberOfX1X2hitsFitted;
     cout << " " << setw(11) << fTableData[row].numberOfSuccessfulIterations;
     cout << " " << setw(11) << fTableData[row].chi2;
     cout << " " << setw(11) << fTableData[row].ErrorCode;
     cout << " " << setw(11) << fTableData[row].alpha;
     cout << " " << setw(11) << fTableData[row].zed;
     cout << " " << setw(11) << fTableData[row].momentum;
     cout << " " << setw(11) << fTableData[row].fittedAlpha;
     cout << " " << setw(11) << fTableData[row].fittedPhi;
     cout << " " << setw(11) << fTableData[row].fittedPhi0;
     cout << " " << setw(11) << fTableData[row].fittedBeta;
     cout << " " << setw(11) << fTableData[row].fittedTheta0;
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].vertex[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectToVertex[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].predictMomentum[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectToPc1[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectToPc2[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectToPc3[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectToTec[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectToPbSc[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectToPbGl[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectToCrk[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectToTof[i0];
     }

     cout << endl;
  }

}

void
dPHDchTrackWrapper::Print(Option_t* option) const
{
   // This version of Print overrides the one in the TObject
   // base class, and provides a way to call Print with no
   // arguments.  If Print(const size_t, const size_t) const
   // could be called with no arguments, there would be an
   // ambiguity.  I hope that this explanation makes sense!

   if (!option || (strlen(option) <= 0) ) {
     // default:  call Print(const size_t, const size_t)
     Print(10, 0);
   } else {
     // non-null option:  call PHTable::Print, for lack of
     // anything better to do ...
     PHTable::Print(option);
   }
}

void
dPHDchTrackWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DPHDCHTRACK_ST* newData = new DPHDCHTRACK_ST[max_rows];
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
dPHDchTrackWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dPHDchTrackWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dPHDchTrackWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dPHDchTrackWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DPHDCHTRACK_ST)) {
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
     fTableData = new DPHDCHTRACK_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dPHDchTrackWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].arm;
        b >> fTableData[i].side;
        b >> fTableData[i].charge;
        b >> fTableData[i].numberOfX1X2hitsFitted;
	b >> fTableData[i].numberOfSuccessfulIterations;
	b >> fTableData[i].chi2;
	b >> fTableData[i].ErrorCode;
        b >> fTableData[i].alpha;
	b >> fTableData[i].zed;
	b >> fTableData[i].momentum;
        b >> fTableData[i].fittedAlpha;
        b >> fTableData[i].fittedPhi;
        b >> fTableData[i].fittedPhi0;
        b >> fTableData[i].fittedBeta;
        b >> fTableData[i].fittedTheta0;
        b.ReadStaticArray(fTableData[i].vertex);
        b.ReadStaticArray(fTableData[i].projectToVertex);
        b.ReadStaticArray(fTableData[i].predictMomentum);
        b.ReadStaticArray(fTableData[i].projectToPc1);
        b.ReadStaticArray(fTableData[i].projectToPc2);
        b.ReadStaticArray(fTableData[i].projectToPc3);
        b.ReadStaticArray(fTableData[i].projectToTec);
        b.ReadStaticArray(fTableData[i].projectToPbSc);
        b.ReadStaticArray(fTableData[i].projectToPbGl);
        b.ReadStaticArray(fTableData[i].projectToCrk);
        b.ReadStaticArray(fTableData[i].projectToTof);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].arm;
        b << fTableData[i].side;
        b << fTableData[i].charge;
        b << fTableData[i].numberOfX1X2hitsFitted;
	b << fTableData[i].numberOfSuccessfulIterations;
	b << fTableData[i].chi2;
	b << fTableData[i].ErrorCode;
        b << fTableData[i].alpha;
	b << fTableData[i].zed;
	b << fTableData[i].momentum;
        b << fTableData[i].fittedAlpha;
        b << fTableData[i].fittedPhi;
        b << fTableData[i].fittedPhi0;
        b << fTableData[i].fittedBeta;
        b << fTableData[i].fittedTheta0;
        b.WriteArray(fTableData[i].vertex,3);
        b.WriteArray(fTableData[i].projectToVertex,3);
        b.WriteArray(fTableData[i].predictMomentum,3);
        b.WriteArray(fTableData[i].projectToPc1,3);
        b.WriteArray(fTableData[i].projectToPc2,3);
        b.WriteArray(fTableData[i].projectToPc3,3);
        b.WriteArray(fTableData[i].projectToTec,3);
        b.WriteArray(fTableData[i].projectToPbSc,3);
        b.WriteArray(fTableData[i].projectToPbGl,3);
        b.WriteArray(fTableData[i].projectToCrk,3);
        b.WriteArray(fTableData[i].projectToTof,3);
     }
   }

}
/* Automatically generated.  Do not edit. */
