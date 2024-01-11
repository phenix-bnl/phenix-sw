#include <emcOMAsciiT.h>
#include <emcTracedFEM.h>
#include <emcDataManager.h>
#include <EmcIndexer.h>
#include <emcTracedValue.h>
#include <emcTofT0FEM.h>
#include <asciitimestamp.h>
#include <dirfilemanip.h>
#include <TSystem.h>
#include <cassert>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include <iostream>
#include <iomanip>

using namespace std;

namespace
{
  bool ReadOneFEMFromFile(std::string filename, emcTracedFEM& fem,
                          int)
  {
    // This method assumes that filename contains data for 144 channels.

    emcDataManager* dm = emcDataManager::GetInstance();

    std::ifstream fin(filename.c_str());

    if ( !fin )
      {
        std::cerr << __FILE__ << ":" << __LINE__
		  << " Cannot open "
		  << filename << std::endl;
        return false;
      }

    if ( dm->GetVerboseLevel() > 0 )
      {
        std::cout << __FILE__ << ":" << __LINE__
		  << " Reading file "
		  << filename << std::endl;
      }

    fem.SetNumberOfChannels(144);

    PHTimeStamp tStart = getTimeStamp(fin);

    int i;
    int thex;
    float theconstant;
    float theslope;

    bool end = false;
    bool oldversion = true;

    while ( fin >> i >> thex >> theconstant >> theslope && !end)
      {

        if (i < 144)
          {
            fem.AddNewItem(i, new emcTracedValue(thex, theconstant, theslope));
          }
        else
          {
            // last 1 or 2 items are used to set the xmax (and xmin for V1)
            if ( i == 144 )
              {
                // xmax
                assert (theconstant == 0);
                assert (theslope == 0);
                fem.SetXmax(thex);
              }
            fin >> i >> thex >> theconstant >> theslope;
            if ( i == 145 )
              {
                assert (theconstant == 0);
                assert (theslope == 0);
                fem.SetXmin(thex);
                oldversion = false;
              }
	    if (!fin.eof())
	      {
		fin >> i >> thex >> theconstant >> theslope;
		if ( i == 146 )
		  {
		    //  std::cout<<"ReadOneFEMFromFile Category "<<fem.GetCategory()<<std::endl;
		    assert(thex==0);
		    assert(theslope==0);
		    assert(strcmp(fem.GetCategory(),"TofT0Bs")==0);
		    (static_cast<emcTofT0FEM&>(fem)).setBBCT0(theconstant);
		  }
	      }
            end = true;
          }
      }

    fin.close();

    PHTimeStamp tEnd;
    tEnd.setToFarFuture();

    fem.SetValidityPeriod(tStart, tEnd);

    if ( oldversion == true && EmcIndexer::isPbScFEM(fem.AbsolutePosition()) )
      {
        // we must update Xmin and Xmax.
        phtime_t tics0 = tStart.getTics();
        fem.SetXmin( tics0 );
        fem.SetXmax( fem.GetXmax() + tics0 );
      }

    return true;
  }

  bool namer(const emcTracedFEM& tracedFEM,
	     int to_or_from,
	     std::string& filename)
  {
    int SN, SM144N;
    
    EmcIndexer::PXSM144_iSiSM144(tracedFEM.AbsolutePosition(), SN, SM144N);

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
    
    std::string category = tracedFEM.GetCategory();
    std::string FileExt;
    std::string wTracerName;

    if ( category == "Gains" || category == "GainsV1" )
      {
	
        dirname += "/Gains/";
        FileExt = "GAINS";
	
        if ( SN < 6 )
	  {
	    wTracerName = "LASER-TOWERS-NORM_ADC";
	  }
        else
	  {
	    wTracerName = "LED(AVY)-TOWERS-NORM_ADC";
	  }
      }
    else if ( category == "TofT0s" || category == "TofT0Bs" )
      {
        dirname += "/ToF/";
        FileExt = "TofT0s";
	
        if ( SN < 6 )
	  {
	    wTracerName = "PHYSICS-TWRS-T0";
	  }
        else
	  {
	    wTracerName = "LED-TOWERS-T0-DRIFT";
	  }
      }
    else if ( category == "TacPeds")
      {
        dirname += "/ToF/";
        FileExt = "TofT0s";
        wTracerName = "PED-TOWERS-TAC-DRIFT";
      }
    else
      {
        std::cerr << __FILE__ << ":" << __LINE__
		  << " Don't know how to handle category "
		  << category << std::endl;
        return false;
      }

    if ( to_or_from )
      {
	if (!createDirectory(dirname))
	  {
	    return false;
	  }
      }
    
    std::ostringstream sfilename;

    sfilename << expand(dirname) << "/"
	      << EmcIndexer::EmcSectorId(SN)
	      << "SM"
	      << SM144N
	      << "."
	      << wTracerName
	     << "."
	      << FileExt;
    
    filename = sfilename.str();

    return true;
  }

  bool reader(emcTracedFEM& tracedFEM, int code)
  {
    if ( code >= 0 )
      {
	int femAbsPosition;
	int idummy;
	emcCalFEM::FEMDecode(code,femAbsPosition,idummy,idummy,idummy);
	if (femAbsPosition!=tracedFEM.AbsolutePosition())
	  {
	    std::cerr << __FILE__ << ":" << __LINE__
		      << " code mismatch with absolutePosition"
		      << std::endl;
	    return false;
	  }
      }

    std::string filename;

    bool ok = namer(tracedFEM,0,filename);

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

    return ReadOneFEMFromFile(filename, tracedFEM, 
			      tracedFEM.AbsolutePosition());
  }

  bool writer(const emcTracedFEM& tracedFEM, int)
  {
    std::string filename;
    bool ok = namer(tracedFEM,1,filename); 
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

    out << tracedFEM.GetStartValTime() << std::endl;

    for ( size_t i = 0; i < tracedFEM.size(); ++i )
      {
	tracedFEM.FirstItem(i);
	emcTracedValue* tv;
	while ( ( tv = tracedFEM.NextItem() ) )
	  {
	    std::ostream::fmtflags oldflags = out.flags();
	    out << std::setw(6) << i << "  ";
	    // use std::istream::fixed instead of std::ios_base::fixed
	    // in order to accomodate gcc-2.95 which uses a
	    // pre-standard I/O library.  Eventually, we'll get rid of
	    // workarounds like this.
	    out.setf(std::istream::scientific);
	    out << std::setw(14) << tv->GetX() << "  ";
	    out << std::setw(14) << tv->GetConstant() << "  ";
	    out << std::setw(14) << tv->GetSlope()
		<< std::endl;
	    out.setf(oldflags);
	  }	
      }
    

    out << "144 " << tracedFEM.GetXmax() << " 0 0" << std::endl;
    out << "145 " << tracedFEM.GetXmin() << " 0 0" << std::endl;

    if ( strcmp(tracedFEM.GetCategory(),"TofT0Bs")==0 && 
	 tracedFEM.Version()>0 )
      {
	out << "146 0 " 
	    << static_cast<const emcTofT0FEM&>(tracedFEM).getBBCT0()
	    << " 0" << std::endl;
      }

    out.close();

    return true;
  }

  emcOMAsciiT<emcTracedFEM> gemcOMTracedFEM("emcOMTracedFEM",
					    "Read/Write emcTracedFEM objects",
					    reader,
					    writer);
}
