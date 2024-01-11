#include "emcRecoModuleRealFactory.h"
#include "mEmcRecoModuleRealYear2.h"
#include "mEmcRecoModuleRealYear3.h"
#include "mEmcRecoModuleRealYear3v1.h"
#include "mEmcRecoModuleRealYear4.h"
#include "PHFlag.h"

#include "RunToYear.h"

#include <cstdlib>
#include <iostream>

using namespace std;



//_____________________________________________________________________________
SubsysReco *
emcRecoModuleRealFactory::create(const PHFlag& flags)
{
  int runnumber = flags.get_IntFlag("RUNNUMBER");

  int year = RunToYear(abs(runnumber));

  if ( year == 1 ) 
    {      
	  //	  return new mEmcRecoModuleRealYear1(flags);
	  return 0;
    }
  else if ( year == 2 ) 
    {
	  return new mEmcRecoModuleRealYear2(flags);	  
    }
  else if ( year >= 3 ) 
    {
      int simu = 0;

      if ( flags.FlagExist("SIMULATIONFLAG") )
	{
	  simu = flags.get_IntFlag("SIMULATIONFLAG");
	}

      if ( simu == 0 )
	{
	  if ( flags.FlagExist("EMCBACKWARD") &&
	       flags.get_IntFlag("EMCBACKWARD") == 1 )
	    {
	      return new mEmcRecoModuleRealYear3(flags);
	    }
	  else if ( year == 3 )
	    {
	      return new mEmcRecoModuleRealYear3v1(flags);
	    }
	  else 
	    {
	      return new mEmcRecoModuleRealYear4(flags);
	    }
	}
    }
  else
    {
      cerr << "<ERROR-FATAL> emcRecoModuleRealFactory::create : runnumber "
	   << runnumber << " lead me to year = " << year << " ?!"
	   << endl;
      return 0;
    }

  cerr << "emcRecoModuleRealFactory::create : failed to find a suitable "
       << " recoModule for those flags " << endl;
  flags.Print();

  return 0;
}
