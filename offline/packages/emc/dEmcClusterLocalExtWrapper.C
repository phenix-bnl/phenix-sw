#include <iostream> // MV 2001/12/12
#include "emcDefines.h" // MV 2001/12/12
#include "dEmcClusterLocalExtWrapper.h"

ClassImp(dEmcClusterLocalExtWrapper);

using namespace std;

dEmcClusterLocalExtWrapper::dEmcClusterLocalExtWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DEMCCLUSTERLOCALEXT_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DEMCCLUSTERLOCALEXT_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DEMCCLUSTERLOCALEXT_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dEmcClusterLocalExt");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dEmcClusterLocalExtWrapper::~dEmcClusterLocalExtWrapper()
{
  delete [] fTableData;
}

void*
dEmcClusterLocalExtWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DEMCCLUSTERLOCALEXT_ST*
dEmcClusterLocalExtWrapper::TableData()
{
  return fTableData;
}

DEMCCLUSTERLOCALEXT_ST&
dEmcClusterLocalExtWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DEMCCLUSTERLOCALEXT_ST&
dEmcClusterLocalExtWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dEmcClusterLocalExtWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DEMCCLUSTERLOCALEXT_ST* newData = new DEMCCLUSTERLOCALEXT_ST[max_rows];
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
dEmcClusterLocalExtWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  } else {
     fTableHeader->nok = n;
  }
}

void
dEmcClusterLocalExtWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dEmcClusterLocalExtWrapper::Streamer(TBuffer &R__b)
{
  // MV 2001/12/12 Incremented dEmcClusterLocalExtWrapper version number
  // 1 -> 2

  // Stream an object of class dEmcClusterLocalExtWrapper.
  // What should be done on output if the table is empty?
  
  if(R__b.IsReading()){

    static bool first=true;

    Version_t R__v=R__b.ReadVersion();
    PHTable::Streamer(R__b);         // Read the table header.

    // MV 2001/12/12 we don't need this check
    //    if (RowSize() != sizeof(DEMCCLUSTERLOCALEXT_ST)) {
    // Sanity check failed.  Need some error message here.
    //      return;
    //    }
    
    // Reallocate the table explicitly here; the size of the data array
    // may be inconsistent with the max. row count variable in the header
    // (since the ROOT I/O default-constructs the former, and reads
    // the header for the latter).
    size_t max_rows = MaxRowCount();
    if (max_rows <= 0) { // Avoid allocating a space of zero size!
      max_rows = 1;
    }
    
    delete [] fTableData;
    fTableData = new DEMCCLUSTERLOCALEXT_ST[max_rows];
    fTableHeader->data_pointer = (long)fTableData;
    
    SetMaxRowCount(max_rows);
    SetType("dEmcClusterLocalExtWrapper");
    
    for(unsigned long i=0; i<RowCount(); i++){

      R__b>>fTableData[i].id;
      R__b>>fTableData[i].runno;
      R__b>>fTableData[i].evno;
      R__b>>fTableData[i].clusno;
      R__b>>fTableData[i].method;
      R__b>>fTableData[i].type;
      R__b>>fTableData[i].arm;
      R__b>>fTableData[i].sector;
      R__b.ReadStaticArray(fTableData[i].xyz);
      R__b.ReadStaticArray(fTableData[i].dxyz);
      R__b>>fTableData[i].e;
      R__b>>fTableData[i].ecore;
      R__b>>fTableData[i].ecorr;
      R__b>>fTableData[i].de;
      R__b>>fTableData[i].tof;
      R__b>>fTableData[i].ecent;
      R__b>>fTableData[i].tofcorr;
      R__b>>fTableData[i].dtof;
      R__b>>fTableData[i].qual;
      
      // MV 2001/12/12
      if(R__v>1){

	R__b>>fTableData[i].deadmap;
	R__b>>fTableData[i].warnmap;

      } else{
	
	fTableData[i].deadmap=0;
	fTableData[i].warnmap=0;

	if(first){

	cerr<<EMC_INFO_MSG<<" dEmcClusterLocalExtWrapper::Streamer() "
	    <<"backward compatibility mode: deadmap, warnmap contain 0"<<endl;
	first=false;

	}
      }
      
      R__b>>fTableData[i].pid;
      R__b>>fTableData[i].prob_photon;
      R__b>>fTableData[i].prob_neuhad;
      R__b>>fTableData[i].chi2;
      R__b>>fTableData[i].nsh;
      R__b>>fTableData[i].chi2_sh;
      R__b>>fTableData[i].prob_photon_sh;
      R__b.ReadStaticArray(fTableData[i].e_sh);
      R__b.ReadStaticArray(fTableData[i].ecorr_sh);
      R__b.ReadStaticArray(fTableData[i].de_sh);
      R__b.ReadStaticArray((float *)fTableData[i].xyz_sh);
      R__b.ReadStaticArray((float *)fTableData[i].dxyz_sh);
      R__b>>fTableData[i].theta;
      R__b>>fTableData[i].phi;
      R__b.ReadStaticArray(fTableData[i].unitv);
      R__b.ReadStaticArray(fTableData[i].ind);
      R__b>>fTableData[i].twrhit;
      R__b>>fTableData[i].tofmin;
      R__b>>fTableData[i].etofmin;
      R__b>>fTableData[i].tofmincorr;
      R__b>>fTableData[i].tofmax;
      R__b>>fTableData[i].etofmax;
      R__b>>fTableData[i].tofmaxcorr;
      R__b>>fTableData[i].tofmean;
      R__b.ReadStaticArray(fTableData[i].disp);
      R__b.ReadStaticArray(fTableData[i].padisp);
      R__b.ReadStaticArray(fTableData[i].partesum);
      R__b.ReadStaticArray(fTableData[i].twrlist);
      R__b>>fTableData[i].e9;
      R__b>>fTableData[i].re9;
      R__b.ReadStaticArray(fTableData[i].yz_cg);

    }
  } else{

    R__b.WriteVersion(IsA());
    PHTable::Streamer(R__b);         // Write the table header.

    for(unsigned long i=0; i<RowCount(); i++){

      R__b<<fTableData[i].id;
      R__b<<fTableData[i].runno;
      R__b<<fTableData[i].evno;
      R__b<<fTableData[i].clusno;
      R__b<<fTableData[i].method;
      R__b<<fTableData[i].type;
      R__b<<fTableData[i].arm;
      R__b<<fTableData[i].sector;
      R__b.WriteArray(fTableData[i].xyz,3);
      R__b.WriteArray(fTableData[i].dxyz,3);
      R__b<<fTableData[i].e;
      R__b<<fTableData[i].ecore;
      R__b<<fTableData[i].ecorr;
      R__b<<fTableData[i].de;
      R__b<<fTableData[i].tof;
      R__b<<fTableData[i].ecent;
      R__b<<fTableData[i].tofcorr;
      R__b<<fTableData[i].dtof;
      R__b<<fTableData[i].qual;
      
      // MV 2001/12/06
      R__b<<fTableData[i].deadmap;
      R__b<<fTableData[i].warnmap;
      
      R__b<<fTableData[i].pid;
      R__b<<fTableData[i].prob_photon;
      R__b<<fTableData[i].prob_neuhad;
      R__b<<fTableData[i].chi2;
      R__b<<fTableData[i].nsh;
      R__b<<fTableData[i].chi2_sh;
      R__b<<fTableData[i].prob_photon_sh;
      R__b.WriteArray(fTableData[i].e_sh,2);
      R__b.WriteArray(fTableData[i].ecorr_sh,2);
      R__b.WriteArray(fTableData[i].de_sh,2);
      R__b.WriteArray((float *)fTableData[i].xyz_sh,6);
      R__b.WriteArray((float *)fTableData[i].dxyz_sh,6);
      R__b<<fTableData[i].theta;
      R__b<<fTableData[i].phi;
      R__b.WriteArray(fTableData[i].unitv,3);
      R__b.WriteArray(fTableData[i].ind,2);
      R__b<<fTableData[i].twrhit;
      R__b<<fTableData[i].tofmin;
      R__b<<fTableData[i].etofmin;
      R__b<<fTableData[i].tofmincorr;
      R__b<<fTableData[i].tofmax;
      R__b<<fTableData[i].etofmax;
      R__b<<fTableData[i].tofmaxcorr;
      R__b<<fTableData[i].tofmean;
      R__b.WriteArray(fTableData[i].disp,2);
      R__b.WriteArray(fTableData[i].padisp,2);
      R__b.WriteArray(fTableData[i].partesum,16);
      R__b.WriteArray(fTableData[i].twrlist,16);
      R__b<<fTableData[i].e9;
      R__b<<fTableData[i].re9;
      R__b.WriteArray(fTableData[i].yz_cg,2);

    }
  }
}
