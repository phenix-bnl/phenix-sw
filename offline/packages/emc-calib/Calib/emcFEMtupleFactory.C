// $Id: emcFEMtupleFactory.C,v 1.8 2010/01/17 21:15:36 phnxemc Exp $

#include "emcFEMtupleFactory.h"
#include "emcGains.h"
#include "emcTacPeds.h"
#include "emcPedestals.h"
#include "emcPedestals5.h"
#include "emcHLRatios.h"
#include "emcLCTofs.h"
#include "emcWalkTofs.h"
#include "emcTofT0s.h"
#include "emcQAs.h"
#include "emcDefines.h"
#include <string>
#include <iostream>

//_____________________________________________________________________________
emcFEMtuple* emcFEMtupleFactory::Create(const char* category)
{
  std::string c = category ;
  if ( c == "Gains" ) return new emcGains() ;
  if ( c == "Pedestals" ) return new emcPedestals() ;
  if ( c == "Pedestals5" ) return new emcPedestals5() ;
  if ( c == "HLRatios" ) return new emcHLRatios() ;
  if ( c == "LCTofs" ) return new emcLCTofs() ;
  if ( c == "WalkTofs" ) return new emcWalkTofs() ;
  if ( c == "TofT0Bs" ) return new emcTofT0s() ;
  if ( c == "QAs" ) return new emcQAs() ;
  if ( c == "TacPeds" ) return new emcTacPeds() ;

  std::cerr << EMC_ERROR_MSG 
	    << " emcFEMtupleFactory::Create : I don't know how to"
	    << " create an emcFEMtuple of category " << c << std::endl ;
  return 0 ;
}
