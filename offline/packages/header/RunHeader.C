#include "RunHeader.h"
#include "phool.h"
#include <iostream>

ClassImp(RunHeader)

using namespace std;

static int nowarning = 0;

RunHeader*
RunHeader::clone() const
{
  cout << "RunHeader::clone() is not implemented in daugther class" << endl;
  return NULL;
}

void
RunHeader::Reset()
{
  cout << PHWHERE << "ERROR Reset() not implemented by daughter class" << endl;
  return ;
}

void
RunHeader::identify(ostream& os) const
{
  os << "identify yourself: virtual RunHeader Object" << endl;
  return ;
}

int
RunHeader::isValid() const
{
  cout << PHWHERE << "isValid not implemented by daughter class" << endl;
  return 0;
}

int
RunHeader::get_RunNumber() const
{
  warning("get_RunNumber()");
  return -9999;
}

void
RunHeader::set_RunNumber(const int /*run*/)
{
  warning("set_RunNumber(const int run)");
  return ;
}

double
RunHeader::get_Bfield() const
{
  warning("get_Bfield()");
  return -9999.9;
}

void
RunHeader::set_Bfield(const double /*rval*/)
{
  warning("set_Bfield(const double rval)");
  return;
}

time_t
RunHeader::get_TimeStart() const
{
  warning("get_TimeStart()");
  return 0;
}

void
RunHeader::set_TimeStart(const time_t /*ival*/)
{
  warning("set_TimeStart(const time_t ival)");
  return;
}

time_t
RunHeader::get_TimeStop() const
{
  warning("get_TimeStop()");
  return 0;
}

void
RunHeader::set_TimeStop(const time_t /*ival*/)
{
  warning("set_TimeStop(const time_t ival)");
  return;
}

int
RunHeader::get_currentNorth() const
{
  warning("get_currentNorth()");
  return -9999;
}

void
RunHeader::set_currentNorth(const int /*icur*/)
{
  warning("set_currentNorth(const int icur)");
  return;
}

int
RunHeader::get_currentSouth() const
{
  warning("get_currentSouth()");
  return -9999;
}

void
RunHeader::set_currentSouth(const int /*icur*/)
{
  warning("set_currentSouth(const int icur)");
  return;
}

int
RunHeader::get_currentCentral() const
{
  warning("get_currentCentral()");
  return -9999;
}

void
RunHeader::set_currentCentral(const int /*icur*/)
{
  warning("set_currentCentral(const int icur)");
  return;
}

int
RunHeader::get_currentInner() const
{
  warning("get_currentInner()");
  return -9999;
}

void
RunHeader::set_currentInner(const int /*icur*/)
{
  warning("set_currentInner(const int icur)");
  return;
}

// 
void
RunHeader::NoWarning(const int i)
{
  if (i > 0)
    {
      cout << "RunHeader: switching virtual warnings OFF" << endl;
      nowarning = i;
    }
  else
    {
      cout << "RunHeader: switching virtual warnings ON" << endl;
      nowarning = 0;
    }
  return ;
}

void
RunHeader::warning(const char *funcname) const
{
  if (! nowarning)
    {
      cout << "Using virtual function RunHeader::" << funcname << endl;
    }
    return ;
  }
