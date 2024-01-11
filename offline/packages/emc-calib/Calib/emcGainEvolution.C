
#include "emcGainEvolution.h"
#include "emcCalibrationDataHelper.h"
#include "emcGainBaseLineCalculator.h"
#include <iostream>
#include <iomanip>
#include "EmcIndexer.h"
#include <cassert>
#include <sstream>
#include <cmath>
#include "TGraphErrors.h"
#include "TFile.h"
#include "emcTimeStamp.h"
#include "emcDataManager.h"
#include "TGraphErrors.h"

namespace
{
  std::string range(int run)
  {
    std::ostringstream rv;
    int r = static_cast<int>(run/1000);
    r *= 1000;
    rv << std::setw(10) << std::setfill('0')
       << r << "_"
       << std::setw(10) << std::setfill('0')
       << r+1000;
    return rv.str();      
  }
}

//_____________________________________________________________________________
emcGainEvolution::BaseLine
emcGainEvolution::RunCheck::get(const std::string& details) const
{
  std::map<std::string,BaseLine>::const_iterator it;
  it = fValues.find(details);
  if ( it != fValues.end() )
    {
      return it->second;
    }
  else
    {
      std::cerr << "RunCheck::get : details=" << details
		<< " does not exist" 
		<< std::endl;
      return BaseLine();
    }
}

//_____________________________________________________________________________
emcGainEvolution::emcGainEvolution(const char* outputfile)
  : fOutputFile(outputfile), 
    fTimeOffset(5), 
    fVerbose(0),
    fHistogrammingType(emcGainEvolution::kNone)
{
  TFile f(outputfile,"RECREATE");
  if (!f.IsOpen())
    {
      throw;
    }
  f.Close();  
}

//_____________________________________________________________________________
emcGainEvolution::~emcGainEvolution()
{
  std::map<std::string,TGraphErrors*>::iterator gip;

  for ( gip = fGraphs.begin(); gip != fGraphs.end(); ++gip ) 
    {
      delete gip->second;
      gip->second=0;
    }
}

//_____________________________________________________________________________
void
emcGainEvolution::createGraph(const std::string& graphname,
			      const std::vector<double>& x,
			      const std::vector<double>& y,
			      const std::vector<double>& xerr,
			      const std::vector<double>& yerr)
{
  std::map<std::string,TGraphErrors*>::iterator it = 
    fGraphs.find(graphname);
  
  if ( it != fGraphs.end() )
    {
      if ( verbose() ) 
	{
	  std::cout << "emcGainEvolution::makeGraphs : graph "
		    << graphname << " already exists. Erasing it."
		    << std::endl;
	}
      delete it->second;
    }

  fGraphs[graphname] 
    = new TGraphErrors(x.size(),&x[0],&y[0],&xerr[0],&yerr[0]);
  fGraphs[graphname]->SetName(graphname.c_str());
  fGraphs[graphname]->SetTitle(graphname.c_str());
}
	
//_____________________________________________________________________________
std::string
emcGainEvolution::graphName(const std::string& process, 
			    const std::string& suffix)
{
  std::string name = "g";
  name += process;
  for ( size_t i = 0; i < name.size(); ++i ) 
    {
      if (name[i]==':')
	{
	  name[i]='_';
	}
      if (name[i]=='-')
	{
	  name.erase(i,1);
	}
    }
  name += "_";
  name += suffix;
  return name;		 
}

//_____________________________________________________________________________
void
emcGainEvolution::setHistogrammingType(emcGainEvolution::EHistogrammingType htype)
{
  if ( htype != emcGainEvolution::kNone )
    {
      emcGainBaseLineCalculator::histogramming(true);
      fHistogrammingType = htype;
    }
  else
    {
      emcGainBaseLineCalculator::histogramming(false);
    }
}

//_____________________________________________________________________________
void
emcGainEvolution::makeGraphs(const std::string& process)
{
  makeGraph_absolute(process);
  makeGraph_wrt_production(process);
  makeGraph_distriShape(process);
}

//_____________________________________________________________________________
void
emcGainEvolution::makeGraph_distriShape(const std::string& process)
{
  // Make graphs of the BaseLine distribution's rms, skewness and kurtosis

  std::vector<double> x;
  std::vector<double> yrms;
  std::vector<double> yskewness;
  std::vector<double> ykurtosis;

  for ( size_t i = 0; i < fRuns.size(); ++i )
    {
      int run = fRuns[i];
      std::map<int,RunCheck>::const_iterator it = fRunChecks.find(run);
      assert( it != fRunChecks.end() );
      BaseLine bl = it->second.get(process);
      x.push_back(run);
      yrms.push_back(bl.error());
      yskewness.push_back(bl.skewness());
      ykurtosis.push_back(bl.kurtosis());
    }

  std::vector<double> zero(x.size());

  createGraph(graphName(process,"rms"),x,yrms,zero,zero);
  createGraph(graphName(process,"skewness"),x,yskewness,zero,zero);
  createGraph(graphName(process,"kurtosis"),x,ykurtosis,zero,zero);
}

//_____________________________________________________________________________
void
emcGainEvolution::makeGraph_absolute(const std::string& process)
{
  // Make a graph of the BaseLine absolute values as a function
  // of run number.

  std::string graphname = graphName(process,"abs");

  std::vector<double> x;
  std::vector<double> xerr;
  std::vector<double> y;
  std::vector<double> yerr;

  for ( size_t i = 0; i < fRuns.size(); ++i )
    {
      int run = fRuns[i];
      std::map<int,RunCheck>::const_iterator it = fRunChecks.find(run);
      assert( it != fRunChecks.end() );
      BaseLine bl = it->second.get(process);
      x.push_back(run);
      xerr.push_back(0.5);
      y.push_back(bl.value());
      yerr.push_back(bl.error());
    }

  createGraph(graphname,x,y,xerr,yerr);
}

//_____________________________________________________________________________
void
emcGainEvolution::makeGraph_wrt_production(const std::string& process)
{
  // Make a graph of the BaseLine relative differences wrt 
  // what was used in Run4 63GeV run data production, assuming
  // this was gainBaseLine computed using details = 0:xmax:-3:-9:-15

  std::string graphname = graphName(process,"rprod");

  std::vector<double> x;
  std::vector<double> xerr;
  std::vector<double> y;
  std::vector<double> yerr;

  for ( size_t i = 0; i < fRuns.size(); ++i )
    {
      int run = fRuns[i];
      std::map<int,RunCheck>::const_iterator it = fRunChecks.find(run);
      assert( it != fRunChecks.end() );
      BaseLine bl = it->second.get(process);
      x.push_back(run);
      xerr.push_back(0.5);
      y.push_back(bl.diff());
      yerr.push_back(bl.diffError());
    }

  createGraph(graphname,x,y,xerr,yerr);
}



//_____________________________________________________________________________
void
emcGainEvolution::print(std::ostream& os) const
{
  std::set<std::string>::const_iterator it;

  for ( size_t i = 0; i < fRuns.size(); ++i ) 
    {
      os << "RUN " << fRuns[i] << std::endl;
      std::map<int,RunCheck>::const_iterator mit = fRunChecks.find(fRuns[i]);
      assert(mit!=fRunChecks.end());
      for ( it = fProcesses.begin(); it != fProcesses.end(); ++it ) 
	{
	  const std::string process = (*it);
	  os << process << " -> ";
	  BaseLine bl = mit->second.get(process);
	  bl.print(os);
	}
    }
}

//_____________________________________________________________________________
void
emcGainEvolution::run()
{
  emcTimeStamp ets;
  ets.SetSource(emcManageable::kDB_Pg);
  emcDataManager* dm = emcDataManager::GetInstance();
  PHTimeStamp ts;

  for ( size_t i = 0; i < fRuns.size(); ++i ) 
    {
      int run = fRuns[i];

      if ( verbose() )
	{
	  std::cout << "Working on run " << run << std::endl;
	}

      emcCalibrationDataHelper* cdh;
	  
      bool ok = dm->Read(ets,run);
      assert(ok==true);
      ts = ets.getTimeStamp().getTics();
      ts += fTimeOffset;
	      
      cdh = new emcCalibrationDataHelper(run,ts,
					 false,
					 emcManageable::kDB_Pg,
					 "pbsc");
	  
      for ( size_t is = 0; is < fSectors.size(); ++is ) 
	{
	  int isector = EmcIndexer::EmcSectorNumber(fSectors[is].c_str());
	  assert(isector>=0 && isector<6);

	  const bool reallySilent = true;

	  float refvalue = 
	    cdh->getGainBaseLine(isector,"value",
				 "0:xmax:-3:-15:-9:92446",reallySilent);
	  float referror = 
	    cdh->getGainBaseLine(isector,"error",
				 "0:xmax:-3:-15:-9:92446",reallySilent);

	  for ( size_t imd = 0; imd < fDetails.size(); ++imd )
	    {
	      const char* details[] = { "xmax","xmin","x" };
	      // First one = xmax is supposed to be the reference.

	      for ( size_t ip = 0; ip < fPercent.size(); ++ip ) 
		{
		  for ( size_t id = 0; id < 3; ++id ) 
		    {
		      float percent = fPercent[ip];
		      std::ostringstream sdetails;
		      sdetails << (static_cast<int>(floorf(percent*100)))
			       << ":" << details[id]
			       << ":" << fDetails[imd];       
		      

		      float value = 
			cdh->getGainBaseLine(isector,"value",
					     sdetails.str().c_str(),
					     reallySilent);
		      float error = 
			cdh->getGainBaseLine(isector,"error",
					     sdetails.str().c_str(),
					     reallySilent);
		      float skewness = 
			cdh->getGainBaseLine(isector,"skewness",
					     sdetails.str().c_str(),
					     reallySilent);
		      float kurtosis = 
			cdh->getGainBaseLine(isector,"kurtosis",
					     sdetails.str().c_str(),
					     reallySilent);
			
		      if ( verbose() > 1 ) 
			{
			  std::cout << "     " << fSectors[is] 
				    << " " << sdetails.str() << std::endl;
			}

		      std::ostringstream sprocess;
		      sprocess << fSectors[is] << ":" << sdetails.str();
		      
		      fProcesses.insert(sprocess.str());
		      
		      float diff = value-refvalue;
		      float diffError = 
			sqrt(error*error+referror*referror)/diff;
		      
		      if ( refvalue != 0)
			{
			  diff = diff*100/refvalue;
			}
		      else
			{
			  diff = 0.0;
			}
		      
		      diffError *= diff;
		      
		      fRunChecks[run].set(sprocess.str(),
					  BaseLine(value,error,
						   diff,diffError,
						   skewness,kurtosis));
		    }
		}
	    }
	}      
      delete cdh;
      save(run);
    }
  
  writeGraphs();
}

//_____________________________________________________________________________
void
emcGainEvolution::save(int run)
{
  if ( fHistogrammingType == kNone )
    {
      return; // no op in this case.
    }

  std::ostringstream filename;
  TFile* f = 0;

  std::string::size_type pos = fOutputFile.find_last_of('.');
  std::string mode = "UPDATE";
  bool make_subdirectories = true;

  if ( fHistogrammingType == kOneFilePerRun )
    {
      filename << fOutputFile.substr(0,pos) << "." << run << ".root";
      mode = "RECREATE";      
      make_subdirectories = false;
    }
  else if ( fHistogrammingType == kOneFilePer1KRange )
    {
      filename << fOutputFile.substr(0,pos) << "." << range(run) << ".root";
      mode = "UPDATE";
    }
  else if ( fHistogrammingType == kOneFileForAll )
    {
      filename << fOutputFile;
      mode = "UPDATE";
    }
  else
    {
      throw;
    }

  std::cout << "<I> emcGainEvolution::save : saving results for run "
	    << run << " into file "
	    << filename.str() << std::endl;

  f = new TFile(filename.str().c_str(),mode.c_str());
  emcGainBaseLineCalculator::write(make_subdirectories);
  f->Write();
  f->Close();
  delete f;
  // clean the current histograms (if we have some, otherwise
  // this is a no op simply)
  emcGainBaseLineCalculator::deleteHistos();
}

//_____________________________________________________________________________
void
emcGainEvolution::writeGraphs()
{
  std::cout << "<I> emcGainEvolution::writeGraphs to " << fOutputFile
	    << std::endl;

  std::set<std::string>::const_iterator it;

  for ( it = fProcesses.begin(); it != fProcesses.end(); ++it ) 
    {
      std::string process = (*it);

      makeGraphs(process);
    }

  TFile f(fOutputFile.c_str(),"UPDATE");

  std::map<std::string,TGraphErrors*>::const_iterator gip;

  for ( gip = fGraphs.begin(); gip != fGraphs.end(); ++gip ) 
    {
      gip->second->Write();
    }

  f.Write();
  f.Close();

  std::cout << "done." << std::endl;
}

