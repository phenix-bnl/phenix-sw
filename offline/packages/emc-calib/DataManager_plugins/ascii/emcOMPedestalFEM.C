#include "emcOMAsciiT.h"
#include "emcPedestalFEM.h"
#include <string>
#include <vector>
#include "emcDataManager.h"
#include "EmcIndexer.h"
#include <cassert>
#include <cstdio>
#include <fstream>
#include "asciitimestamp.h"

namespace 
{
  bool reader(emcPedestalFEM& pedFEM, int code)
  {  
    int femAbsPosition;
    int pinNumber;
    int idummy;
    if (code<0)
      {
	std::cerr << __FILE__ << ":" << __LINE__ << " Sorry, I need "
		  << " a full code to be able to deduce the pinnumber"
		  << std::endl;
	return false;
      }
    emcCalFEM::FEMDecode(code,femAbsPosition,pinNumber,idummy,idummy);
    assert(femAbsPosition==pedFEM.AbsolutePosition());
    //    std::cout << "code=" << code
    //	      << " pos=" << femAbsPosition
    //	      << " pin=" << pinNumber
    //	      << std::endl;

    pedFEM.Reset();
    std::vector<std::string> valid_types;
    
    pedFEM.GetListOfValidTypes(valid_types);    
        
    int SN, SM144N;
    
    EmcIndexer::PXSM144_iSiSM144(femAbsPosition , SN, SM144N);
    
    char filename[1024];
    const int nchannel_per_file = 144; 
    const int namu_per_channel = 64;
    PHTimeStamp begin;
    PHTimeStamp insert;

    emcDataManager* dm = emcDataManager::GetInstance();

    for ( size_t ped_type = 0; ped_type < valid_types.size(); ped_type++) 
      {
	if ( valid_types[ped_type] == "TACDEV" ) 
	  {
	    // perfectly valid, but not a persistent type, skip it.
	    continue;
	  }

	// build the filename
	sprintf(filename,
		"%s/PEDESTALS/%sSM%d.%s",
		dm->GetSourceDir(),
		EmcIndexer::EmcSectorId(SN),
		SM144N,
		//		pinNumber, 
		valid_types[ped_type].c_str());
	//FIXME: get the pinNumber from gdb.dat in case it is present...
	
	std::ifstream in(filename);
	if (!in) 
	  {
	    std::cerr << __FILE__ << ":" << __LINE__
		      << " Cannot open " 
		      << filename << std::endl;
	    return false;
	  }
	
	begin = getTimeStamp(in);
	insert = getTimeStamp(in);

	int amu;
	
	emcPedestalFEM::AmuVector amuv(namu_per_channel);    
	// loop over the channels
	for ( int i = 0; i < nchannel_per_file; i++ ) 
	  {	  
	    // loop over all the amu cells of this channel
	    for ( int AMU = 0; AMU < namu_per_channel; AMU++ ) 
	      {
		in >> amu;
		amuv[AMU] = amu;
	      } 
	    pedFEM.AppendOneChannel(valid_types[ped_type].c_str(),amuv);
	  }
	in.close();
      }
    
    PHTimeStamp end;
    end.setToFarFuture();
    
    pedFEM.SetValidityPeriod(begin,end);
    
    return true;
  }

  emcOMAsciiT<emcPedestalFEM> gemcOMPedestalFEM("emcOMPedestal",
						"Read emcPedestalFEM objects",
						reader);
}
