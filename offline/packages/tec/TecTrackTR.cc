
#include <iostream>
#include "TecTrackTR.hh"

ClassImp(TecTrackTR)

static int shutup = 0;

void 
TecTrackTR::virtual_warning(const char *funcname) const 
{
  if (!shutup)
    {
      std::cout << "TecTrackTR: " << funcname << " is virtual" << std::endl;
    }
  return;
}

void
TecTrackTR::ShutUp(const int i)
{
  shutup = i;
  return;
}

