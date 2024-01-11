#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include <iostream>

ClassImp(emcTowerContainer)

  using namespace std;

//_____________________________________________________________________________
emcTowerContainer::~emcTowerContainer()
{
}

//_____________________________________________________________________________
emcTowerContainer& 
emcTowerContainer::operator=(const emcTowerContainer&)
{
  return *this;
}
  
//_____________________________________________________________________________
int
emcTowerContainer::isValid() const
{
  cerr << "emcTowerContainer::isValid : should be implemented in daughter class"
       << endl;
  return 0;
}

//_____________________________________________________________________________
void
emcTowerContainer::identify(ostream&) const
{
  cerr << "emcTowerContainer::identify : should be implemented in daughter class"
       << endl;
}

//_____________________________________________________________________________
void
emcTowerContainer::print(ostream& os, int level) const
{
  for ( unsigned int i = 0; i < size() ; ++i ) 
    {
      getTower(i)->print(os,level);
    }
}

//_____________________________________________________________________________
void
emcTowerContainer::Reset()
{
  cerr << "emcTowerContainer::Reset : should be implemented in daughter class"
       << endl;
}
