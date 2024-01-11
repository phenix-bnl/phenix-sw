#include <mpcRawContentV4.h>
#include <recoConsts.h>
#include <limits.h>

ClassImp(mpcRawContentV4)

using namespace std;

mpcRawContentV4::mpcRawContentV4()
{
  ch     = -9999;
  tdc    = SHRT_MAX;
  adc    = -9999.;
  fquality = 0;
}

mpcRawContentV4::mpcRawContentV4(const mpcRawContent &m)
{
  ch     = m.get_ch();
  tdc    = m.get_tdc();
  adc    = m.get_adc();
  fquality    = m.get_fquality();
}


mpcRawContent& mpcRawContentV4::operator=(const mpcRawContent &rhs)
{
  if ( this == &rhs ) return *this;
  
  ch     = rhs.get_ch();
  tdc    = rhs.get_tdc();
  adc    = rhs.get_adc();
  fquality    = rhs.get_fquality();

  return *this;
}

mpcRawContent& mpcRawContentV4::operator+=(const mpcRawContent &rhs)
{
  if ( ch != rhs.get_ch() )
    {
      cerr << PHWHERE << " ERROR, adding different mpc channels! " << ch << " " << rhs.get_ch() << endl;
      return *this;
    }

  adc += rhs.get_adc();

  // pick earlier time
  if ( tdc>rhs.get_tdc() ) tdc = rhs.get_tdc();

  return *this;
}

mpcRawContent& mpcRawContentV4::operator+(const mpcRawContent &rhs)
{
  mpcRawContentV4 *new_rawcontent = new mpcRawContentV4;
  *new_rawcontent = *this;

  *new_rawcontent += rhs;

  return *new_rawcontent;
}

void mpcRawContentV4::print(std::ostream& out)
{
  out << ch << "\t" << tdc << "\t" << adc << "\t" << fquality << endl;
}
