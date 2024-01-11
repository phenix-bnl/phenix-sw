#include "emcObjectFiller.h"
#include "emcObjectFillerRegistry.h"

ClassImp(emcObjectFiller)

//_____________________________________________________________________________
emcObjectFiller::emcObjectFiller()
{
  registerMe();
}

//_____________________________________________________________________________
void
emcObjectFiller::registerMe(void)
{
  emcObjectFillerRegistry::add(this);
}
