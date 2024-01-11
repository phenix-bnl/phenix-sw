#include "ACCInclusiveNanoCutsv1.h"
#include "PHCentralTrack.h"

ClassImp(ACCInclusiveNanoCutsv1)

PHBoolean ACCInclusiveNanoCutsv1::CentralTrackOK(PHCentralTrack* php, const unsigned int itrk) {

  // sanity checks, itrk is unsigned int <0 check not needed
  if (itrk>= php->get_npart()) return False;
  if (php->get_the0(itrk)<0)             return False;

  // actual cut requires aerindex or "swapped" aerindex is valid:
  if (php->get_aerindex(itrk)  < 0 && 
      php->get_aersindex(itrk) < 0 )
    {
      return False;
    }

  return True;
}







