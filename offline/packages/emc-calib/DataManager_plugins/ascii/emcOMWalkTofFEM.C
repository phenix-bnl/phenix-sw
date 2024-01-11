#include "emcOMAsciiT.h"
#include "emcWalkTofFEM.h"
#include "emcDataManager.h"
#include "EmcIndexer.h"
#include "asciitimestamp.h"
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include "dirfilemanip.h"

namespace
{
   bool namer(const emcWalkTofFEM& hlr,
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

    dirname += "/ToF";

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
	      << SM144N << ".TOF_WALK";

    filename = sfilename.str();

    return true;
  }

  bool writer(const emcWalkTofFEM& tofFEM, int)
  {
    std::string filename;
    bool ok = namer(tofFEM,1,filename); 
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
	std::cerr << __FILE__ << " Could not create file " 
		  << filename << std::endl;
	return false;
      }

    out << tofFEM.GetStartValTime() << std::endl;
    out << PHTimeStamp() << std::endl;

    for ( size_t i = 0; i < tofFEM.size(); ++i )
      {
	out << i << " " 
	    << tofFEM.getValue(i,0) << " "
	    << tofFEM.getValue(i,1) << " "
	    << 0 << std::endl;
      }

    out.close();

    emcDataManager* dm = emcDataManager::GetInstance();
    if ( dm->GetVerboseLevel() )
      {
	std::cout << "emcOMWalkTofFEM written to " 
		  << filename << std::endl;
      }

    return true;
  }

  bool reader(emcWalkTofFEM& tofFEM, int code)
  {
    if ( code >= 0 )
      {
	int femAbsPosition;
	int idummy;
	emcCalFEM::FEMDecode(code,femAbsPosition,idummy,idummy,idummy);
	if (femAbsPosition!=tofFEM.AbsolutePosition())
	  {
	    std::cerr << __FILE__ << ":" << __LINE__
		      << " code mismatch with absolutePosition"
		      << std::endl;
	    return false;
	  }
      }

    std::string filename;

    bool ok = namer(tofFEM,0,filename);

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

    std::ifstream fs(filename.c_str());

    PHTimeStamp begin = getTimeStamp(fs);
    PHTimeStamp insert = getTimeStamp(fs);

    PHTimeStamp end;
    end.setToFarFuture();

    int index = 0;
    int nchannelPerFile = 144;

    while ( ( index < (nchannelPerFile - 1)) && !fs.eof() )
      {
        float value1;
        float value2;
	float dummy;
        fs >> index >> value1 >> value2 >> dummy;
        tofFEM.AppendOneChannel(value1, value2);
      }

    fs.close();
    tofFEM.SetValidityPeriod(begin, end);

    return true;
  }

  emcOMAsciiT<emcWalkTofFEM> 
  gemcOMWalkTofFEM("emcOMWalkTofFEM",
		   "Read/Write emcWalkTofFEM objects from/to files",
		   reader,
		   writer);
}
