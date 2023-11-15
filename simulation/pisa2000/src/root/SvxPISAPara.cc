#include "SvxPISAPara.h"
#include <cmath>
#include "phool.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Base virtual class                                                   //
// Implementation of SVX Parameters in PISA                             //
// Sasha Lebedev (lebedev@iastate.edu)                                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(SvxPISAPara)

void SvxPISAPara::identify(std::ostream& os) const
{
  os << "virtual SvxPISAPara object";
  return ;
}

void SvxPISAPara::Reset()
{
  std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter class" << std::endl;
  return ;
}

int SvxPISAPara::isValid() const
{
  virtual_warning("isValid()");
  return 0;
}

void SvxPISAPara::virtual_warning(const char *funcsname) const
{
  std::cout << PHWHERE << " " << funcsname << " is virtual, doing nothing" << std::endl;
  return ;
}


