#include "EmcHitMapEntry.h"
#include <iostream>

using namespace std;

EmcHitMapEntry::EmcHitMapEntry()
{
  id = -1;
  emcrawtdc = -9999;
  emcrawadc = -9999;
  emcrawadclg = -9999;
  return;
}

void
EmcHitMapEntry::identify(ostream &os) const
{
  os << "EmcHitMapEntry: id: " << id 
     << ", emcrawtdc: " << emcrawtdc
     << ", emcrawadc: " << emcrawadc
     << ", emcrawadclg: " << emcrawadclg
     << endl;
  return;
}
