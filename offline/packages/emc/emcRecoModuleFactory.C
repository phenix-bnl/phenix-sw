
#include <PHFlag.h>
#include "emcRecoModuleSimFactory.h"
#include "emcRecoModuleRealFactory.h"
#include "emcRecoModuleFactory.h"


//_____________________________________________________________________________
SubsysReco *
emcRecoModuleFactory::create(const PHFlag& flags)
{
  if ( flags.FlagExist("SIMULATIONFLAG") && flags.get_IntFlag("SIMULATIONFLAG") )
    return emcRecoModuleSimFactory::create(flags);
  else 
    return emcRecoModuleRealFactory::create(flags);
}
