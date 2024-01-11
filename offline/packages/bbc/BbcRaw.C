#include <iostream>
#include "phool.h"
#include "BbcRaw.h"
#include "BbcReturncodes.h"

using namespace std;

ClassImp(BbcRaw)

void BbcRaw::identify(ostream& os) const
{
  os << "virtual BbcRaw object";
  return ;
}

void BbcRaw::Reset()
{
  cout << PHWHERE << "ERROR Reset() not implemented by daughter class" << endl;
  return ;
}

int BbcRaw::isValid() const
{
  virtual_warning("isValid()");
  return 0;
}

void BbcRaw::set_npmt(const short ival)
{
  virtual_warning("set_npmt(const short ival)");
  return ;
}

short BbcRaw::get_npmt() const
{
  virtual_warning("get_npmt()");
  return BBC_INVALID_SHORT;
}

short BbcRaw::get_Pmt(const short iPmt) const
{
  virtual_warning("get_Pmt(const short iPmt)");
  return BBC_INVALID_SHORT;
}

short BbcRaw::get_Adc(const short iPmt) const
{
  virtual_warning("get_Adc(const short iPmt)");
  return BBC_INVALID_SHORT;
}

short BbcRaw::get_Tdc0(const short iPmt) const
{
  virtual_warning("get_Tdc0(const short iPmt)");
  return BBC_INVALID_SHORT;
}

short BbcRaw::get_Tdc1(const short iPmt) const
{
  virtual_warning("get_Tdc1(const short iPmt)");
  return BBC_INVALID_SHORT;
}

void BbcRaw::AddBbcRawHit(const short pmt, const short adc,
                          const short tdc0, const short tdc1,
                          const short ipmt)
{
  virtual_warning("AddBbcRawHit(const short pmt, const short adc, const short tdc0, const short tdc1, const short ipmt)");
  return ;
}

void BbcRaw::AddBbcRawHit(const short pmt, const short adc,
                          const short tdc0, const short ipmt)
{
  virtual_warning("AddBbcRawHit(const short pmt, const short adc, const short tdc0, const short ipmt)");
  return ;
}

void BbcRaw::virtual_warning(const char *funcsname) const
{
  cout << "BbcRaw::" << funcsname << " is virtual, doing nothing" << endl;
  return ;
}
