#include <iostream>
#include "DchHitLineTable.hh"

ClassImp(DchHitLineTable)

void  DchHitLineTable::Reset()
{
  std::cout << PHWHERE 
	    << " Reset not implemented, danger of Event mixing!!" 
	    << std::endl;
  return;
}

int DchHitLineTable::isValid() const
{
  std::cout << PHWHERE 
	    << "isValid() not implemented by daughter class" 
	    << std::endl;
  return 0;
}
