#include "PreviousEvent.h"
#include <iostream>

ClassImp(PreviousEvent)

using namespace std;

void
PreviousEvent::Reset()
{
  cout << PHWHERE << "ERROR Reset() not implemented by daughter class" << endl;
  return ;
}


void
PreviousEvent::identify(ostream& os) const
{
  os << "identify yourself: virtual PreviousEvent Object" << endl;
  return ;
}

/// isValid returns non zero if object contains valid data

int
PreviousEvent::isValid() const
{
  cout << PHWHERE << "isValid not implemented by daughter class" << endl;
  return 0;
}
