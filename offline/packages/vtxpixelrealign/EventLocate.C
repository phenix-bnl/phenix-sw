#include "EventLocate.h"

#include <cstdlib>

using namespace std;


void EventLocate::Reset()
{
  // we don't want to reset this for each event
  // so just return here
  return;
}

void EventLocate::identify(ostream& out) const
{
  out << "identify yourself: I am an base EventLocate Object" << endl;
  return;
}

pair<map<int, int>::const_iterator, map<int, int>::const_iterator>
EventLocate::beginend()
{
  cout << "calling base class which is bad" << endl;
  exit(1);
}
