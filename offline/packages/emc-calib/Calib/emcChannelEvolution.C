#include "emcChannelEvolution.h"

#include <iostream>
#include <cassert>
#include "EmcIndexer.h"
#include "emcTimeStamp.h"
#include "emcDataManager.h"
#include "emcCalibrationDataHelper.h"
#include "emcTracedValue.h"
#include "emcTracedFEM.h"
#include "emcGainFEM.h"
#include "emcTracedFEMMerger.h"
#include "emcTracedFEMPlotter.h"
#include "phool.h"
#include "TGraph.h"
#include <sstream>
#include <iomanip>
#include <iterator>
#include <algorithm>
#include "PdbBankManagerFactory.hh"
#include "PdbCalBankIterator.hh"
#include "PdbCalBank.hh"

namespace
{
  void dump(const emcTracedFEM& tfem, int channel)
  {
    tfem.FirstItem(channel);
    emcTracedValue* tv = 0;
    while ( ( tv = tfem.NextItem() ) )
      {
        std::cout << (*tv);
      }
  }

  void scale(emcTracedFEM& tfem, float BL)
  {
    for ( int channel = 0; channel < 144; ++channel)
      {
        tfem.FirstItem(channel);
        emcTracedValue* tv = 0;
        while ( ( tv = tfem.NextItem() ) )
          {
            tv->Set(tv->GetX(), tv->GetConstant() / BL, tv->GetSlope() / BL,
                    tv->isConstant());
          }
      }
  }

  static const int nfems = 108;
}



//_____________________________________________________________________________
emcChannelEvolution::emcChannelEvolution(emcManageable::EStorage ds)
  :
  fCDH(0),
  fDataSource(ds)
{
  fGains.resize(nfems, 0);
}

//_____________________________________________________________________________
emcChannelEvolution::~emcChannelEvolution()
{
  for ( size_t i = 0; i < fGains.size(); ++i )
    {
      delete fGains[i];
    }
  delete fCDH;
}

//_____________________________________________________________________________
void
emcChannelEvolution::dump(int ifem, int channel)
{
  if ( ifem >= 0 && ifem < static_cast<int>(fGains.size()) )
    {
      emcTracedFEM* g = fGains[ifem];
      if (g)
	{
	  ::dump(*g,channel);
	}
      else
	{
	  std::cout << PHWHERE << "Do not have this gain object"
		    << std::endl;
	}
    }
  else
    {
      std::cout << PHWHERE << "Do not know this fem/channel" << std::endl;
    }
}

//_____________________________________________________________________________
TGraph*
emcChannelEvolution::graph(int ifem, int channel)
{
  if ( ifem >= 0 && ifem < static_cast<int>(fGains.size()) )
    {
      emcTracedFEM* g = fGains[ifem];
      if (!g)
	{
	  g = read(ifem);
	}
      if (g)
	{
	  return graph(ifem,channel,g->GetXmin());
	}
    }
  return 0;
}

//_____________________________________________________________________________
TGraph*
emcChannelEvolution::graph(int ifem, int channel, time_t tics0)
{
  if ( ifem >= 0 && ifem < static_cast<int>(fGains.size()) )
    {
      emcTracedFEM* g = fGains[ifem];
      if (!g)
	{
	  g = read(ifem);
	}

      if (g)
	{
	  emcTracedFEMPlotter plot(*g,tics0);
	  TGraph* graph = plot.CopyGraph(channel);
	  std::ostringstream name;
	  name << "FEM" << std::setw(3) << std::setfill('0') << ifem
	       << "CH" << std::setw(3) << std::setfill('0') << channel;
	  graph->SetName(name.str().c_str());
	  return graph;
	}
    }
  return 0;
}

//_____________________________________________________________________________
void
emcChannelEvolution::produce(int run1, int run2, const char* femdetails)
{
  assert(run1<=run2);

  // First goals is to get, from [run1,run2] range, the list
  // of validity periods as they stand in the database.

  PdbBankManager* bm = PdbBankManagerFactory::instance().create(emcManageable::GetStorageName(fDataSource));
  
  if (!bm) 
    {
      std::cerr << PHWHERE << "Could not get bank manager" << std::endl;
      return;
    }

  PdbCalBankIterator* it = bm->getIterator();
  if (!it)
    {
      std::cerr << PHWHERE << "Could not get calbank iterator" << std::endl;
      return;
    }

  it->init("calib.emc.Gains",0);

  emcDataManager* dm = emcDataManager::GetInstance();
  emcTimeStamp ets;
  ets.SetSource(fDataSource);
  
  bool ok = dm->Read(ets,run1);
  if (!ok)
    {
      std::cerr << "Could not find time stamp for run" << run1
		<< std::endl;
      return;
    }

  PHTimeStamp t1 = ets.getTimeStamp();

  ok = dm->Read(ets,run2);
  if (!ok)
    {
      std::cerr << "Could not find time stamp for run" << run2
		<< std::endl;
      return;
    }

  PHTimeStamp t2 = ets.getTimeStamp();

  it->setStartValTimeLimits(t1,t2);

  it->print();

  PdbCalBank* bank = 0;

  std::vector<PHTimeStamp> starts;

  // From the validity periods, we only keep the starts.
  while ( ( bank = it->next() ) )
    {
      starts.push_back(bank->getStartValTime());
    }

  delete it;

  // Sort the starttimes
  std::sort(starts.begin(),starts.end());

  // Remove duplicates, if any
  starts.erase(std::unique(starts.begin(),starts.end()),starts.end());

  // Now loop over all startvaltimes, and collect the gains, and
  // merged them.

  for ( size_t i = 0; i < starts.size(); ++i )
    {
      PHTimeStamp ts = starts[i];

      std::cout << "Collecting CDH @ " << ts << std::endl;
      delete fCDH;
      int dummy=0;
      fCDH = new emcCalibrationDataHelper(dummy,ts,
					  false,
					  fDataSource);
      
      for ( int ifem = 0; ifem < nfems; ++ifem )
	{
	  const emcTracedFEM* gains =
	    dynamic_cast<const emcTracedFEM*>
	    (fCDH->getCalibration(ifem, "Gains"));
	  assert(gains != 0);
	  
	  int isector, ism144;
	  
	  EmcIndexer::PXSM144_iSiSM144(ifem, isector, ism144);
	  
	  float BL = fCDH->getGainBaseLine(isector,"value",femdetails);
	  
	  if (ifem==0)
	    {
	      std::cout << ifem << " gains= (xmax="
			<< PHTimeStamp(gains->GetXmax())
			<< " (tics=" << gains->GetXmax() << "))" << std::endl;
	    }
	  
	  if ( !fGains[ifem] )
	    {
	      fGains[ifem] = gains->clone();
	      scale(*(fGains[ifem]), BL);
	    }
	  else
	    {
	      emcTracedFEM* tmp = fGains[ifem]->clone();
	      delete fGains[ifem];
	      emcTracedFEM* cgains = gains->clone();
	      scale(*cgains, BL);
	      fGains[ifem] = emcTracedFEMMerger::merge(*tmp, *cgains);
	      delete tmp;
	      delete cgains;
	    }
	} // loop over fems
    } // loop over validity periods
}

//_____________________________________________________________________________
emcTracedFEM*
emcChannelEvolution::read(int ifem)
{
  emcDataManager* dm = emcDataManager::GetInstance();
  std::string ddir = dm->GetSourceDir();
  dm->SetSourceDir(fSourceDir.c_str());

  PHTimeStamp dummy;

  emcTracedFEM* g = new emcGainFEM(ifem);

  g->SetSource(emcManageable::kFile_ASCII);
  bool ok = dm->Read(*g,dummy);
  if (!ok)
    {
      std::cerr << PHWHERE << " Cannot read gains for fem " << ifem
		<< std::endl;
      return 0;
    }

  fGains[ifem] = g;

  dm->SetSourceDir(ddir.c_str());
  return g;
}

//_____________________________________________________________________________
bool
emcChannelEvolution::read(const char* inputdir)
{
  fSourceDir = inputdir;

  for ( size_t i = 0; i < fGains.size(); ++i ) 
    {
      delete fGains[i];      
    }

  fGains.resize(nfems,0);

  return true;
}

//_____________________________________________________________________________
bool
emcChannelEvolution::save(const char* outputdir)
{
  emcDataManager* dm = emcDataManager::GetInstance();
  std::string ddir = dm->GetDestinationDir();
  dm->SetDestinationDir(outputdir);
  for ( size_t i = 0; i < fGains.size(); ++i ) 
    {
      emcTracedFEM* g = fGains[i];
      if (g)
	{
	  g->SetDestination(emcManageable::kFile_ASCII);
	  bool ok = dm->Write(*g);
	  if (!ok)
	    {
	      std::cerr << PHWHERE << " Cannot save gains "
			<< std::endl;
	      return false;
	    }
	}
    }
  dm->SetDestinationDir(ddir.c_str());
  return true;
}
