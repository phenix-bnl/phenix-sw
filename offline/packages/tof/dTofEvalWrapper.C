#include <cstring>
#include <iostream>
#include <iomanip>
#include "dTofEvalWrapper.h"

ClassImp(dTofEvalWrapper);

using namespace std;

dTofEvalWrapper::dTofEvalWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DTOFEVAL_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DTOFEVAL_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DTOFEVAL_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dTofEval");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dTofEvalWrapper::dTofEvalWrapper(const dTofEvalWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DTOFEVAL_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DTOFEVAL_ST));
  SetType("dTofEval");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dTofEvalWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dTofEvalWrapper&
dTofEvalWrapper::operator=(const dTofEvalWrapper& source)
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

dTofEvalWrapper::~dTofEvalWrapper()
{
  delete [] fTableData;
}

DTOFEVAL_ST*
dTofEvalWrapper::TableData()
{
  return fTableData;
}

DTOFEVAL_ST&
dTofEvalWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DTOFEVAL_ST&
dTofEvalWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dTofEvalWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "id";
  cout << " " << setw(11) << "slatid";
  cout << " " << setw(11) << "sector";
  cout << " " << setw(11) << "side";
  cout << " " << setw(11) << "panel";
  cout << " " << setw(11) << "slat";
  cout << " " << setw(11) << "rawid";
  cout << " " << setw(11) << "recid";
  cout << " " << setw(11) << "gdigiid";
  cout << " " << setw(11) << "nslathit";
  cout << " " << setw(11) << "hits_seq";
  cout << " " << setw(11) << "nrectrack";
  cout << " rawqvc[0]";
  cout << " rawqvc[1]";
  cout << " rawtvc[0]";
  cout << " rawtvc[1]";
  cout << " " << setw(11) << "dtof";
  cout << " " << setw(11) << "rectof";
  cout << " " << setw(11) << "mctof";
  cout << " " << setw(11) << "deloss";
  cout << " " << setw(11) << "receloss";
  cout << " " << setw(11) << "mceloss";
  cout << " " << setw(11) << "drphi";
  cout << " " << setw(11) << "dz";
  cout << " recpos[0]";
  cout << " recpos[1]";
  cout << " recpos[2]";
  cout << " mcpos[0]";
  cout << " mcpos[1]";
  cout << " mcpos[2]";
  cout << " " << setw(11) << "mcpid";
  cout << " " << setw(11) << "r_vertex";
  cout << " " << setw(11) << "z_vertex";
  cout << " " << setw(11) << "phi_vertex";
  cout << " vertex[0]";
  cout << " vertex[1]";
  cout << " vertex[2]";
  cout << " " << setw(11) << "p_vertex";
  cout << " " << setw(11) << "ptheta";
  cout << " " << setw(11) << "pphi";
  cout << " " << setw(11) << "idparent";
  cout << " " << setw(11) << "r_verorg";
  cout << " " << setw(11) << "z_verorg";
  cout << " " << setw(11) << "phi_verorg";
  cout << " " << setw(11) << "p_verorg";
  cout << " " << setw(11) << "pthetaorg";
  cout << " " << setw(11) << "pphiorg";
  cout << " " << setw(11) << "idorigin";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].id;
     cout << " " << setw(11) << fTableData[row].slatid;
     cout << " " << setw(11) << fTableData[row].sector;
     cout << " " << setw(11) << fTableData[row].side;
     cout << " " << setw(11) << fTableData[row].panel;
     cout << " " << setw(11) << fTableData[row].slat;
     cout << " " << setw(11) << fTableData[row].rawid;
     cout << " " << setw(11) << fTableData[row].recid;
     cout << " " << setw(11) << fTableData[row].gdigiid;
     cout << " " << setw(11) << fTableData[row].nslathit;
     cout << " " << setw(11) << fTableData[row].hits_seq;
     cout << " " << setw(11) << fTableData[row].nrectrack;
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].rawqvc[i0];
     }
     for(int i0=0; i0<2; i0++) {
        cout << " " << setw(11) << fTableData[row].rawtvc[i0];
     }
     cout << " " << setw(11) << fTableData[row].dtof;
     cout << " " << setw(11) << fTableData[row].rectof;
     cout << " " << setw(11) << fTableData[row].mctof;
     cout << " " << setw(11) << fTableData[row].deloss;
     cout << " " << setw(11) << fTableData[row].receloss;
     cout << " " << setw(11) << fTableData[row].mceloss;
     cout << " " << setw(11) << fTableData[row].drphi;
     cout << " " << setw(11) << fTableData[row].dz;
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].recpos[i0];
     }
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].mcpos[i0];
     }
     cout << " " << setw(11) << fTableData[row].mcpid;
     cout << " " << setw(11) << fTableData[row].r_vertex;
     cout << " " << setw(11) << fTableData[row].z_vertex;
     cout << " " << setw(11) << fTableData[row].phi_vertex;
     for(int i0=0; i0<3; i0++) {
        cout << " " << setw(11) << fTableData[row].vertex[i0];
     }
     cout << " " << setw(11) << fTableData[row].p_vertex;
     cout << " " << setw(11) << fTableData[row].ptheta;
     cout << " " << setw(11) << fTableData[row].pphi;
     cout << " " << setw(11) << fTableData[row].idparent;
     cout << " " << setw(11) << fTableData[row].r_verorg;
     cout << " " << setw(11) << fTableData[row].z_verorg;
     cout << " " << setw(11) << fTableData[row].phi_verorg;
     cout << " " << setw(11) << fTableData[row].p_verorg;
     cout << " " << setw(11) << fTableData[row].pthetaorg;
     cout << " " << setw(11) << fTableData[row].pphiorg;
     cout << " " << setw(11) << fTableData[row].idorigin;

     cout << endl;
  }

}

void
dTofEvalWrapper::Print(Option_t* option) const
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
dTofEvalWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DTOFEVAL_ST* newData = new DTOFEVAL_ST[max_rows];
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
dTofEvalWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dTofEvalWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dTofEvalWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dTofEvalWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DTOFEVAL_ST)) {
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
     fTableData = new DTOFEVAL_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dTofEvalWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].id;
        b >> fTableData[i].slatid;
        b >> fTableData[i].sector;
        b >> fTableData[i].side;
        b >> fTableData[i].panel;
        b >> fTableData[i].slat;
        b >> fTableData[i].rawid;
        b >> fTableData[i].recid;
        b >> fTableData[i].gdigiid;
        b >> fTableData[i].nslathit;
        b >> fTableData[i].hits_seq;
        b >> fTableData[i].nrectrack;
        b.ReadStaticArray(fTableData[i].rawqvc);
        b.ReadStaticArray(fTableData[i].rawtvc);
        b >> fTableData[i].dtof;
        b >> fTableData[i].rectof;
        b >> fTableData[i].mctof;
        b >> fTableData[i].deloss;
        b >> fTableData[i].receloss;
        b >> fTableData[i].mceloss;
        b >> fTableData[i].drphi;
        b >> fTableData[i].dz;
        b.ReadStaticArray(fTableData[i].recpos);
        b.ReadStaticArray(fTableData[i].mcpos);
        b >> fTableData[i].mcpid;
        b >> fTableData[i].r_vertex;
        b >> fTableData[i].z_vertex;
        b >> fTableData[i].phi_vertex;
        b.ReadStaticArray(fTableData[i].vertex);
        b >> fTableData[i].p_vertex;
        b >> fTableData[i].ptheta;
        b >> fTableData[i].pphi;
        b >> fTableData[i].idparent;
        b >> fTableData[i].r_verorg;
        b >> fTableData[i].z_verorg;
        b >> fTableData[i].phi_verorg;
        b >> fTableData[i].p_verorg;
        b >> fTableData[i].pthetaorg;
        b >> fTableData[i].pphiorg;
        b >> fTableData[i].idorigin;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].id;
        b << fTableData[i].slatid;
        b << fTableData[i].sector;
        b << fTableData[i].side;
        b << fTableData[i].panel;
        b << fTableData[i].slat;
        b << fTableData[i].rawid;
        b << fTableData[i].recid;
        b << fTableData[i].gdigiid;
        b << fTableData[i].nslathit;
        b << fTableData[i].hits_seq;
        b << fTableData[i].nrectrack;
        b.WriteArray(fTableData[i].rawqvc,2);
        b.WriteArray(fTableData[i].rawtvc,2);
        b << fTableData[i].dtof;
        b << fTableData[i].rectof;
        b << fTableData[i].mctof;
        b << fTableData[i].deloss;
        b << fTableData[i].receloss;
        b << fTableData[i].mceloss;
        b << fTableData[i].drphi;
        b << fTableData[i].dz;
        b.WriteArray(fTableData[i].recpos,3);
        b.WriteArray(fTableData[i].mcpos,3);
        b << fTableData[i].mcpid;
        b << fTableData[i].r_vertex;
        b << fTableData[i].z_vertex;
        b << fTableData[i].phi_vertex;
        b.WriteArray(fTableData[i].vertex,3);
        b << fTableData[i].p_vertex;
        b << fTableData[i].ptheta;
        b << fTableData[i].pphi;
        b << fTableData[i].idparent;
        b << fTableData[i].r_verorg;
        b << fTableData[i].z_verorg;
        b << fTableData[i].phi_verorg;
        b << fTableData[i].p_verorg;
        b << fTableData[i].pthetaorg;
        b << fTableData[i].pphiorg;
        b << fTableData[i].idorigin;
     }
   }

}
/* Automatically generated.  Do not edit. */
