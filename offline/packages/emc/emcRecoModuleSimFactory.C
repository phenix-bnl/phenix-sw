#include <emcRecoModuleSimFactory.h>
#include <mEmcRecoModuleSimulationYear1.h>
#include <mEmcRecoModuleSimulationYear3.h>
#include <EmcRecoModuleSimulationYear8.h>
#include <PHFlag.h>

#include <RunToYear.h>

#include <cstdlib>
#include <iostream>

using namespace std;



//_____________________________________________________________________________
SubsysReco *
emcRecoModuleSimFactory::create(const PHFlag& flags)
{
  int runnumber = flags.get_IntFlag("RUNNUMBER");

  int year = RunToYear(abs(runnumber));


  // magic flag is set, using new emc response code
  if ( flags.FlagExist("EMCSIMULATIONV2") )
    return new EmcRecoModuleSimulationYear8();


  if ( year == 1 ) 
    {      
	  return new mEmcRecoModuleSimulationYear1(flags);
    }
  else if ( year == 2 ) 
    {
	  return 0;//new mEmcRecoModuleSimulationYear2(flags);
    }
  else if ( year >= 3 ) 
    {
      int simu = 0;

      if ( flags.FlagExist("SIMULATIONFLAG") )
	{
	  simu = flags.get_IntFlag("SIMULATIONFLAG");
	}

      if ( simu == 2 ) 
	{
	  return new mEmcRecoModuleSimulationYear3(flags);
	}
    }
  else
    {
      cerr << "<ERROR-FATAL> emcRecoModuleSimFactory::create : runnumber "
	   << runnumber << " lead me to year = " << year << " ?!"
	   << endl;
      return 0;
    }

  cerr << "emcRecoModuleSimFactory::create : failed to find a suitable "
       << "recoModule for those flags " << endl;
  flags.Print();

  return 0;
}
