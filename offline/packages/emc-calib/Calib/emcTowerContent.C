#include "emcTowerContent.h"
#include <iostream>

ClassImp(emcTowerContent)

static int shutup = 0;

//_____________________________________________________________________________
emcTowerContent::~emcTowerContent()
{
}

//_____________________________________________________________________________
emcTowerContent&
emcTowerContent::operator=(const emcTowerContent&)
{
  return *this;
}

//_____________________________________________________________________________
bool
emcTowerContent::hasReference(void) const
{
  return false;
}  

//_____________________________________________________________________________
int 
emcTowerContent::HGPP(void) const
{
  return HGPre()-HGPost(); 
}

//_____________________________________________________________________________
void
emcTowerContent::identify(std::ostream& /*os*/) const
{
  std::cerr << "emcTowerContent::identify : "
	    << "should be implemented in daughter class"
	    << std::endl;
}

//_____________________________________________________________________________
bool
emcTowerContent::isReference(void) const
{
  return false;
}

//_____________________________________________________________________________
int
emcTowerContent::isValid() const
{
  return 0;
}

//_____________________________________________________________________________
int
emcTowerContent::LGPP(void) const
{
  return LGPre()-LGPost();
}

//_____________________________________________________________________________
emcTowerContent*
emcTowerContent::Reference(void) const
{
  return 0;
}

//_____________________________________________________________________________
void
emcTowerContent::ShutUp(const int i)
{
  shutup = i;
}

//_____________________________________________________________________________
void
emcTowerContent::warning(const char* method) const
{ 
  if (!shutup)
    {
      std::cerr << __FILE__ << ":" 
		<< " method " << method 
		<< " IS NOT IMPLEMENTED in daughter "
		<< " class !" << std::endl;
    }
}
