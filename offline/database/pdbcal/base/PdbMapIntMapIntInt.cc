#include "PdbMapIntMapIntInt.hh"
#include <phool.h>

#include <iostream>

using namespace std;


PdbMapIntMapIntInt::PdbMapIntMapIntInt()
 {}

PdbMapIntMapIntInt::~PdbMapIntMapIntInt() { TheMap.clear(); }

void PdbMapIntMapIntInt::Clear(Option_t *) { TheMap.clear(); }

void PdbMapIntMapIntInt::print() const {
  if (TheMap.empty())
    {
      cout << "No Entries in Map" << endl;
      return;
    }
  else 
    {
      cout << "Map Size = " << TheMap.size() << endl;
      map<int, map <int, int> >::const_iterator eviter;
      map <int, int>::const_iterator chipiter;
      for (eviter = TheMap.begin(); eviter !=   TheMap.end(); ++eviter)
	{
	  cout << "Jump Event number: " << eviter->first << endl;
	  for (chipiter = eviter->second.begin(); chipiter != eviter->second.end(); ++chipiter)
	    {
	      cout << "chip " << chipiter->first << " jump: " << chipiter->second << endl;
	    }
	} 
    }
  return;
}


