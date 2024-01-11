#include <iostream>
#include <Lvl2SnglOut.h>
#include <phool.h>

ClassImp(Lvl2SnglOut)

void Lvl2SnglOut::identify(std::ostream& os) const
{
  os << "identify yourself: I am the VIRTUAL Lvl2SnglOut object" << std::endl;
}

void Lvl2SnglOut::Reset()
{
  std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function"
	    << std::endl;
}

void Lvl2SnglOut::Clear(const Option_t *opt)
{
  std::cout << PHWHERE << "ERROR: Clear() not implemented by daughter function"
	    << std::endl;
}

