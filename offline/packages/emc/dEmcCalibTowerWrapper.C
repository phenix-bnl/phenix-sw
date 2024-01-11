#include <iostream>
#include "dEmcCalibTowerWrapper.h"
#include <iostream>
#include "emcDefines.h"

ClassImp(dEmcCalibTowerWrapper);

using namespace std;

dEmcCalibTowerWrapper::dEmcCalibTowerWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DEMCCALIBTOWER_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DEMCCALIBTOWER_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DEMCCALIBTOWER_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dEmcCalibTower");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dEmcCalibTowerWrapper::~dEmcCalibTowerWrapper()
{
  delete [] fTableData;
}

void*
dEmcCalibTowerWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DEMCCALIBTOWER_ST*
dEmcCalibTowerWrapper::TableData()
{
  return fTableData;
}

DEMCCALIBTOWER_ST&
dEmcCalibTowerWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DEMCCALIBTOWER_ST&
dEmcCalibTowerWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dEmcCalibTowerWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DEMCCALIBTOWER_ST* newData = new DEMCCALIBTOWER_ST[max_rows];
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
dEmcCalibTowerWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  } else {
     fTableHeader->nok = n;
  }
}

void
dEmcCalibTowerWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dEmcCalibTowerWrapper::Streamer(TBuffer &R__b)
{
  // MV 2001/12/11 Function significantly modified

  // Stream an object of class dEmcCalibTowerWrapper.
  // What should be done on output if the table is empty?
  
  if(R__b.IsReading()){
    
    Version_t R__v=R__b.ReadVersion();
    
    static bool first=true;
    
    PHTable::Streamer(R__b);         // Read the table header.
    
    // MV 2001/12/11 Now I understand that adding data members to the
    // DEMCCALIBTOWER_ST structure and not incrementing the class version
    // of dEmcCalibTowerWrapper was a big mistake.
    // So, we finally decided to increment the version number.
    // Distinguish between "subversions" of version 1 is tricky.
    // This can be done by looking at rowsize which gives us the number
    // of bytes in the DEMCCALIBTOWER_ST structure. It changed like this:
    // 32 bytes (version 1)
    // 36 bytes (version 1)
    // 44 bytes (version 1)
    // 48 bytes (version 2)
    
    size_t rowsize=RowSize();
    
    // Reallocate the table explicitly here; the size of the data array
    // may be inconsistent with the max. row count variable in the header
    // (since the ROOT I/O default-constructs the former, and reads
    // the header for the latter).
    size_t max_rows = MaxRowCount();
    if (max_rows <= 0) { // Avoid allocating a space of zero size!
      max_rows = 1;
    }
    
    delete [] fTableData;
    fTableData = new DEMCCALIBTOWER_ST[max_rows];
    fTableHeader->data_pointer = (long)fTableData;
    
    SetMaxRowCount(max_rows);
    SetType("dEmcCalibTowerWrapper");
    
    for(unsigned long i=0; i<RowCount(); i++){
      
      R__b>>fTableData[i].id;
      R__b>>fTableData[i].hwkey;
      R__b>>fTableData[i].swkey;
      R__b>>fTableData[i].type;
      R__b>>fTableData[i].arm;
      R__b>>fTableData[i].sector;
      R__b.ReadStaticArray(fTableData[i].ind);
      R__b>>fTableData[i].ecal;
      R__b>>fTableData[i].tof;
      
      if(R__v>1){
	
	R__b>>fTableData[i].deadmap;
	R__b>>fTableData[i].warnmap;
	R__b>>fTableData[i].adc;
	R__b>>fTableData[i].tac;
	
	
      } else if(R__v==1 && rowsize==44){
	
	R__b>>fTableData[i].deadmap;
	fTableData[i].warnmap=0;
	R__b>>fTableData[i].adc;
	R__b>>fTableData[i].tac;
	
	if(first==true){
	  
	  cerr<<EMC_INFO_MSG<<" dEmcCalibTowerWrapper::Streamer() "
	      <<"backward compatibility mode: warnmap contains 0"<<endl;
	  first=false;
	  
	}
      } else if(R__v==1 && rowsize==36){
	
	R__b>>fTableData[i].deadmap;
	fTableData[i].warnmap=0;
	fTableData[i].adc=0.;
	fTableData[i].tac=0.;
	
	if(first==true){
	  
	  cerr<<EMC_INFO_MSG<<" dEmcCalibTowerWrapper::Streamer() "
	      <<"backward compatibility mode: warnmap, adc, tdc contain 0"<<endl;
	  first=false;
	  
	}
      } else if(R__v==1 && rowsize==32){
	
	fTableData[i].deadmap=0;
	fTableData[i].warnmap=0;
	fTableData[i].adc=0.;
	fTableData[i].tac=0.;

	if(first==true){

	cerr<<EMC_INFO_MSG<<" dEmcCalibTowerWrapper::Streamer() "
	    <<"backward compatibility mode: deadmap, warnmap, adc, tdc contain 0"<<endl;
	first=false;

	}
      } else{
	
	//=====> should never get here!!
	
	cerr<<EMC_ERROR_MSG<<" dEmcCalibTowerWrapper::Streamer():"
	    <<" RowSize() does not match dEmcCalibTowerWrapper version."
	    <<" Table will be empty."
	    <<endl;
	SetRowCount(0) ;
	return;
	
      } // end checking class versions
    } // end loop on table rows
  } else{
    
    R__b.WriteVersion(IsA());
    PHTable::Streamer(R__b);         // Write the table header.
    
    for(unsigned long i=0; i<RowCount(); i++){
      
      R__b<<fTableData[i].id;
      R__b<<fTableData[i].hwkey;
      R__b<<fTableData[i].swkey;
      R__b<<fTableData[i].type;
      R__b<<fTableData[i].arm;
      R__b<<fTableData[i].sector;
      R__b.WriteArray(fTableData[i].ind,2);
      R__b<<fTableData[i].ecal;
      R__b<<fTableData[i].tof;
      R__b<<fTableData[i].deadmap;
      
      // MV 2001/12/04
      R__b<<fTableData[i].warnmap;
      
      // MV 2001/09/26
      R__b<<fTableData[i].adc;
      R__b<<fTableData[i].tac;
      
    } // end loop on table rows
  }
}
