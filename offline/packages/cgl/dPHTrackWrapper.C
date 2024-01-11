//INCLUDECHECKER: Removed this line: #include <cstdlib>
#include <cstring>
#include <iostream>
#include <iomanip>
#include <dPHTrackWrapper.h>

using namespace std;

ClassImp(dPHTrackWrapper)

using namespace std;

dPHTrackWrapper::dPHTrackWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DPHTRACK_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DPHTRACK_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DPHTRACK_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dPHTrack");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dPHTrackWrapper::dPHTrackWrapper(const dPHTrackWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DPHTRACK_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DPHTRACK_ST));
  SetType("dPHTrack");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dPHTrackWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dPHTrackWrapper&
dPHTrackWrapper::operator=(const dPHTrackWrapper& source)
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

dPHTrackWrapper::~dPHTrackWrapper()
{
  delete [] fTableData;
}

DPHTRACK_ST*
dPHTrackWrapper::TableData()
{
  return fTableData;
}

DPHTRACK_ST&
dPHTrackWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DPHTRACK_ST&
dPHTrackWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dPHTrackWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "trackIndex";
  cout << " " << setw(11) << "arm";
  cout << " " << setw(11) << "ifIntersectVtx";
  cout << " " << setw(11) << "ifIntersectDch";
  cout << " " << setw(11) << "ifIntersectPc1";
  cout << " " << setw(11) << "ifIntersectPc2";
  cout << " " << setw(11) << "ifIntersectPc3";
  cout << " " << setw(11) << "ifIntersectCrk";
  cout << " " << setw(11) << "ifIntersectTec";
  cout << " " << setw(11) << "ifIntersectTof";
  cout << " " << setw(11) << "ifIntersectPbsc";
  cout << " " << setw(11) << "ifIntersectPbgl";
  cout << " projectionVtx[0]";
  cout << " projectionVtx[1]";
  cout << " projectionVtx[2]";
  cout << " projectionDch[0]";
  cout << " projectionDch[1]";
  cout << " projectionDch[2]";
  cout << " projectionPc1[0]";
  cout << " projectionPc1[1]";
  cout << " projectionPc1[2]";
  cout << " projectionPc2[0]";
  cout << " projectionPc2[1]";
  cout << " projectionPc2[2]";
  cout << " projectionPc3[0]";
  cout << " projectionPc3[1]";
  cout << " projectionPc3[2]";
  cout << " projectionCrk[0]";
  cout << " projectionCrk[1]";
  cout << " projectionCrk[2]";
  cout << " projectionTec[0]";
  cout << " projectionTec[1]";
  cout << " projectionTec[2]";
  cout << " projectionTof[0]";
  cout << " projectionTof[1]";
  cout << " projectionTof[2]";
  cout << " projectionPbSc[0]";
  cout << " projectionPbSc[1]";
  cout << " projectionPbSc[2]";
  cout << " projectionPbGl[0]";
  cout << " projectionPbGl[1]";
  cout << " projectionPbGl[2]";
  cout << " errorVtx[0]";
  cout << " errorVtx[1]";
  cout << " errorVtx[2]";
  cout << " errorDch[0]";
  cout << " errorDch[1]";
  cout << " errorDch[2]";
  cout << " errorPc1[0]";
  cout << " errorPc1[1]";
  cout << " errorPc1[2]";
  cout << " errorPc2[0]";
  cout << " errorPc2[1]";
  cout << " errorPc2[2]";
  cout << " errorPc3[0]";
  cout << " errorPc3[1]";
  cout << " errorPc3[2]";
  cout << " errorCrk[0]";
  cout << " errorCrk[1]";
  cout << " errorCrk[2]";
  cout << " errorTec[0]";
  cout << " errorTec[1]";
  cout << " errorTec[2]";
  cout << " errorTof[0]";
  cout << " errorTof[1]";
  cout << " errorTof[2]";
  cout << " errorPbSc[0]";
  cout << " errorPbSc[1]";
  cout << " errorPbSc[2]";
  cout << " errorPbGl[0]";
  cout << " errorPbGl[1]";
  cout << " errorPbGl[2]";
  cout << " directionVtx[0]";
  cout << " directionVtx[1]";
  cout << " directionVtx[2]";
  cout << " directionDch[0]";
  cout << " directionDch[1]";
  cout << " directionDch[2]";
  cout << " directionPc1[0]";
  cout << " directionPc1[1]";
  cout << " directionPc1[2]";
  cout << " directionPc2[0]";
  cout << " directionPc2[1]";
  cout << " directionPc2[2]";
  cout << " directionPc3[0]";
  cout << " directionPc3[1]";
  cout << " directionPc3[2]";
  cout << " directionCrk[0]";
  cout << " directionCrk[1]";
  cout << " directionCrk[2]";
  cout << " directionTec[0]";
  cout << " directionTec[1]";
  cout << " directionTec[2]";
  cout << " directionTof[0]";
  cout << " directionTof[1]";
  cout << " directionTof[2]";
  cout << " directionPbSc[0]";
  cout << " directionPbSc[1]";
  cout << " directionPbSc[2]";
  cout << " directionPbGl[0]";
  cout << " directionPbGl[1]";
  cout << " directionPbGl[2]";
  cout << " " << setw(11) << "crkPathLength";
  cout << " " << setw(11) << "tofPathLength";
  cout << " " << setw(11) << "emcPathLength";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].trackIndex;
     cout << " " << setw(11) << fTableData[row].arm;
     cout << " " << setw(11) << fTableData[row].ifIntersectVtx;
     cout << " " << setw(11) << fTableData[row].ifIntersectDch;
     cout << " " << setw(11) << fTableData[row].ifIntersectPc1;
     cout << " " << setw(11) << fTableData[row].ifIntersectPc2;
     cout << " " << setw(11) << fTableData[row].ifIntersectPc3;
     cout << " " << setw(11) << fTableData[row].ifIntersectCrk;
     cout << " " << setw(11) << fTableData[row].ifIntersectTec;
     cout << " " << setw(11) << fTableData[row].ifIntersectTof;
     cout << " " << setw(11) << fTableData[row].ifIntersectPbsc;
     cout << " " << setw(11) << fTableData[row].ifIntersectPbgl;
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectionVtx[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectionDch[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectionPc1[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectionPc2[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectionPc3[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectionCrk[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectionTec[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectionTof[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectionPbSc[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].projectionPbGl[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].errorVtx[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].errorDch[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].errorPc1[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].errorPc2[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].errorPc3[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].errorCrk[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].errorTec[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].errorTof[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].errorPbSc[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].errorPbGl[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].directionVtx[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].directionDch[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].directionPc1[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].directionPc2[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].directionPc3[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].directionCrk[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].directionTec[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].directionTof[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].directionPbSc[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].directionPbGl[i0];
     }
     cout << " " << setw(11) << fTableData[row].crkPathLength;
     cout << " " << setw(11) << fTableData[row].tofPathLength;
     cout << " " << setw(11) << fTableData[row].emcPathLength;

     cout << endl;
  }

}

void
dPHTrackWrapper::Print(Option_t* option) const
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
dPHTrackWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DPHTRACK_ST* newData = new DPHTRACK_ST[max_rows];
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
dPHTrackWrapper::SetRowCount(const size_t& n)
{
  if ((int) n > fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if ((int) n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dPHTrackWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dPHTrackWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dPHTrackWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DPHTRACK_ST)) {
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
     fTableData = new DPHTRACK_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dPHTrackWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].trackIndex;
        b >> fTableData[i].arm;
        b >> fTableData[i].ifIntersectVtx;
        b >> fTableData[i].ifIntersectDch;
        b >> fTableData[i].ifIntersectPc1;
        b >> fTableData[i].ifIntersectPc2;
        b >> fTableData[i].ifIntersectPc3;
        b >> fTableData[i].ifIntersectCrk;
        b >> fTableData[i].ifIntersectTec;
        b >> fTableData[i].ifIntersectTof;
        b >> fTableData[i].ifIntersectPbsc;
        b >> fTableData[i].ifIntersectPbgl;
        b.ReadStaticArray(fTableData[i].projectionVtx);
        b.ReadStaticArray(fTableData[i].projectionDch);
        b.ReadStaticArray(fTableData[i].projectionPc1);
        b.ReadStaticArray(fTableData[i].projectionPc2);
        b.ReadStaticArray(fTableData[i].projectionPc3);
        b.ReadStaticArray(fTableData[i].projectionCrk);
        b.ReadStaticArray(fTableData[i].projectionTec);
        b.ReadStaticArray(fTableData[i].projectionTof);
        b.ReadStaticArray(fTableData[i].projectionPbSc);
        b.ReadStaticArray(fTableData[i].projectionPbGl);
        b.ReadStaticArray(fTableData[i].errorVtx);
        b.ReadStaticArray(fTableData[i].errorDch);
        b.ReadStaticArray(fTableData[i].errorPc1);
        b.ReadStaticArray(fTableData[i].errorPc2);
        b.ReadStaticArray(fTableData[i].errorPc3);
        b.ReadStaticArray(fTableData[i].errorCrk);
        b.ReadStaticArray(fTableData[i].errorTec);
        b.ReadStaticArray(fTableData[i].errorTof);
        b.ReadStaticArray(fTableData[i].errorPbSc);
        b.ReadStaticArray(fTableData[i].errorPbGl);
        b.ReadStaticArray(fTableData[i].directionVtx);
        b.ReadStaticArray(fTableData[i].directionDch);
        b.ReadStaticArray(fTableData[i].directionPc1);
        b.ReadStaticArray(fTableData[i].directionPc2);
        b.ReadStaticArray(fTableData[i].directionPc3);
        b.ReadStaticArray(fTableData[i].directionCrk);
        b.ReadStaticArray(fTableData[i].directionTec);
        b.ReadStaticArray(fTableData[i].directionTof);
        b.ReadStaticArray(fTableData[i].directionPbSc);
        b.ReadStaticArray(fTableData[i].directionPbGl);
        b >> fTableData[i].crkPathLength;
        b >> fTableData[i].tofPathLength;
        b >> fTableData[i].emcPathLength;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].trackIndex;
        b << fTableData[i].arm;
        b << fTableData[i].ifIntersectVtx;
        b << fTableData[i].ifIntersectDch;
        b << fTableData[i].ifIntersectPc1;
        b << fTableData[i].ifIntersectPc2;
        b << fTableData[i].ifIntersectPc3;
        b << fTableData[i].ifIntersectCrk;
        b << fTableData[i].ifIntersectTec;
        b << fTableData[i].ifIntersectTof;
        b << fTableData[i].ifIntersectPbsc;
        b << fTableData[i].ifIntersectPbgl;
        b.WriteArray(fTableData[i].projectionVtx,3);
        b.WriteArray(fTableData[i].projectionDch,3);
        b.WriteArray(fTableData[i].projectionPc1,3);
        b.WriteArray(fTableData[i].projectionPc2,3);
        b.WriteArray(fTableData[i].projectionPc3,3);
        b.WriteArray(fTableData[i].projectionCrk,3);
        b.WriteArray(fTableData[i].projectionTec,3);
        b.WriteArray(fTableData[i].projectionTof,3);
        b.WriteArray(fTableData[i].projectionPbSc,3);
        b.WriteArray(fTableData[i].projectionPbGl,3);
        b.WriteArray(fTableData[i].errorVtx,3);
        b.WriteArray(fTableData[i].errorDch,3);
        b.WriteArray(fTableData[i].errorPc1,3);
        b.WriteArray(fTableData[i].errorPc2,3);
        b.WriteArray(fTableData[i].errorPc3,3);
        b.WriteArray(fTableData[i].errorCrk,3);
        b.WriteArray(fTableData[i].errorTec,3);
        b.WriteArray(fTableData[i].errorTof,3);
        b.WriteArray(fTableData[i].errorPbSc,3);
        b.WriteArray(fTableData[i].errorPbGl,3);
        b.WriteArray(fTableData[i].directionVtx,3);
        b.WriteArray(fTableData[i].directionDch,3);
        b.WriteArray(fTableData[i].directionPc1,3);
        b.WriteArray(fTableData[i].directionPc2,3);
        b.WriteArray(fTableData[i].directionPc3,3);
        b.WriteArray(fTableData[i].directionCrk,3);
        b.WriteArray(fTableData[i].directionTec,3);
        b.WriteArray(fTableData[i].directionTof,3);
        b.WriteArray(fTableData[i].directionPbSc,3);
        b.WriteArray(fTableData[i].directionPbGl,3);
        b << fTableData[i].crkPathLength;
        b << fTableData[i].tofPathLength;
        b << fTableData[i].emcPathLength;
     }
   }

}
/* Automatically generated.  Do not edit. */
