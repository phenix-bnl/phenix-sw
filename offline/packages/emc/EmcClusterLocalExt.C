#include "EmcClusterLocalExt.h"

ClassImp(EmcClusterLocalExt)

short EmcClusterLocalExt::get_type(const unsigned int iclus) const
{
  if ( get_arm(iclus) == -9999 ) return -9999;

  if ( get_arm(iclus) == 0 ) return 0;	// PBSC

  if ( get_sector(iclus) <= 1 ) return 1;
  else                     return 0;

  return -9999;
}

