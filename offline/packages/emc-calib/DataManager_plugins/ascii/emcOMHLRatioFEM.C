#include "emcOMAsciiT.h"
#include "emcHLRatioFEM.h"
#include "emcDataManager.h"
#include "EmcIndexer.h"
#include <fstream>
#include <iostream>
#include <cassert>
#include "asciitimestamp.h"
#include "dirfilemanip.h"
#include <sstream>

namespace
{

  bool namer(const emcHLRatioFEM& hlr,
	     int to_or_from,
	     std::string& filename)
  {
    int SN, SM144N;
    
    EmcIndexer::PXSM144_iSiSM144(hlr.AbsolutePosition(), SN, SM144N);

    emcDataManager* dm = emcDataManager::GetInstance();

    std::string dirname;

    if ( to_or_from == 0 )
      {
	dirname = dm->GetSourceDir();
      }
    else
      {
	dirname = dm->GetDestinationDir();
      }

    dirname += "/HLRatio";

    if ( to_or_from )
      {
	if (!createDirectory(dirname))
	  {
	    return false;
	  }
      }
    
    std::ostringstream sfilename;

    sfilename << expand(dirname) << "/"
	      << EmcIndexer::EmcSectorId(SN) << "SM"
	      << SM144N << ".HLR";

    filename = sfilename.str();

    return true;
  }

  bool reader(emcHLRatioFEM& hlr, int code)
  {
    if ( code >= 0 )
      {
	int femAbsPosition;
	int idummy;
	emcCalFEM::FEMDecode(code,femAbsPosition,idummy,idummy,idummy);
	if (femAbsPosition!=hlr.AbsolutePosition())
	  {
	    std::cerr << __FILE__ << ":" << __LINE__
		      << " code mismatch with absolutePosition"
		      << std::endl;
	    return false;
	  }
      }

    std::string filename;

    bool ok = namer(hlr,0,filename);

    if (!ok)
      {
	return false;
      }

    if (!checkFile(filename))
      {
	std::cerr << __FILE__ << ":" << __LINE__ 
		  << " Cannot open file " << filename
		  << std::endl;
	return false;
      }

    std::ifstream fin(filename.c_str()) ;

    PHTimeStamp begin = getTimeStamp(fin);

    PHTimeStamp insert = getTimeStamp(fin);

    PHTimeStamp end;
    end.setToFarFuture();

    assert(hlr.GetNumberOfChannels() == 0);

    int i;
    int index = 0;
    int nchannelPerFile = 144;

    // Loop aver the channels
    for ( i = 0; i < nchannelPerFile; i++ )
      {
        float average;
        float rms;
        float intercept;
        float slope;
        fin >> index >> average >> rms >> intercept >> slope;
        hlr.AppendOneChannel(average, rms, intercept, slope);
      }

    fin.close();

    hlr.SetValidityPeriod(begin, end);

    return true;
  }

  bool writer(const emcHLRatioFEM& hlr, int)
  {
    std::string filename;
    bool ok = namer(hlr,1,filename); 
    if (!ok) 
      {
	return false;
      }
    
    if (checkFile(filename))
      {
	std::cerr << __FILE__ << ":" << __LINE__
		  << " File " << filename
		  << " is on the way. Remove it first"
		  << std::endl;
	return false;
      }

    std::ofstream out(filename.c_str());
    if (!out.good())
      {
	std::cerr << __FILE__ << ":" << __LINE__
		  << " Could not create file " << filename
		  << std::endl;
	return false;
      }

    out << hlr.GetStartValTime() << std::endl;
    out << PHTimeStamp() << std::endl;

    for (size_t i = 0; i < hlr.size(); ++i ) 
      {
	out << i << " "
	    << hlr.getValueFast(i,0) << " "
	    << hlr.getValueFast(i,1) << " "
	    << hlr.getValueFast(i,2) << " "
	    << hlr.getValueFast(i,3) << std::endl;
      }
    out.close();
    
    return true;
  }
  
  emcOMAsciiT<emcHLRatioFEM> gemcOMHLRatioFEM("emcOMHLRatio",
					      "Read emcHLRatioFEM objects",
					      reader,
					      writer);
}
