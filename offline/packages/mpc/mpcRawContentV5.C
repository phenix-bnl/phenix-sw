#include <mpcRawContentV5.h>
#include <recoConsts.h>
#include <limits.h>

ClassImp(mpcRawContentV5)

using namespace std;

mpcRawContentV5::mpcRawContentV5()
{
  ch     = -9999;
  tdc    = SHRT_MAX;
  tdc1    = SHRT_MAX;
  tdc2    = SHRT_MAX;
  adc    = -9999.;
  adc1    = -9999.;
  adc2    = -9999.;
  zsm     = 0.0; 
  fquality = 0;
}

mpcRawContentV5::mpcRawContentV5(const mpcRawContent &m)
{
  ch     = m.get_ch();
  tdc    = m.get_tdc();
  tdc1    = m.get_tdc1();
  tdc2    = m.get_tdc2();
  adc    = m.get_adc();
  adc1    = m.get_adc1();
  adc2    = m.get_adc2();
  zsm     = m.get_ZSM(); 
  fquality    = m.get_fquality();
}


mpcRawContent& mpcRawContentV5::operator=(const mpcRawContent &rhs)
{
  if ( this == &rhs ) return *this;
  
  ch     = rhs.get_ch();
  tdc    = rhs.get_tdc();
  tdc1    = rhs.get_tdc1();
  tdc2    = rhs.get_tdc2();
  adc    = rhs.get_adc();
  adc1    = rhs.get_adc1();
  adc2    = rhs.get_adc2();
  zsm     = rhs.get_ZSM(); 
  fquality    = rhs.get_fquality();

  return *this;
}

mpcRawContent& mpcRawContentV5::operator+=(const mpcRawContent &rhs)
{
  if ( ch != rhs.get_ch() )
    {
      cerr << PHWHERE << " ERROR, adding different mpc channels! " << ch << " " << rhs.get_ch() << endl;
      return *this;
    }

  adc += rhs.get_adc();

  // pick earlier time
  if ( tdc>rhs.get_tdc() ) tdc = rhs.get_tdc();

  // Ignore multiple pulses for now
  
  adc1 = adc; 
  adc2 = 0.0; 
  tdc1 = tdc;
  tdc2 = -1.0; 
  zsm = 0.0; // no way to re-evaluate

  return *this;
}

mpcRawContent& mpcRawContentV5::operator+(const mpcRawContent &rhs)
{
  mpcRawContentV5 *new_rawcontent = new mpcRawContentV5;
  *new_rawcontent = *this;

  *new_rawcontent += rhs;

  return *new_rawcontent;
}

void mpcRawContentV5::print(std::ostream& out)
{
  out << ch << "\t" << tdc << "\t" << adc << "\t" << fquality << endl;
}
