#include <mpcRawContentV1.h>
#include <recoConsts.h>

ClassImp(mpcRawContentV1)

using namespace std;

mpcRawContentV1::mpcRawContentV1()
{
  ch     = -9999;
  tdc    = -9999;
  lopost = -9999;
  lopre  = -9999;
}

mpcRawContentV1::mpcRawContentV1(const mpcRawContent &m)
{
  ch     = m.get_ch();
  tdc    = m.get_tdc();
  lopost = m.get_lopost();
  lopre  = m.get_lopre();
}

mpcRawContent& mpcRawContentV1::operator=(const mpcRawContent &rhs)
{
  if ( this == &rhs ) return *this;
  
  ch     = rhs.get_ch();
  tdc    = rhs.get_tdc();
  lopost = rhs.get_lopost();
  lopre  = rhs.get_lopre();

  return *this;
}

mpcRawContent& mpcRawContentV1::operator+=(const mpcRawContent &rhs)
{
  if ( ch != rhs.get_ch() )
    {
      cerr << PHWHERE << " ERROR, adding different mpc channels! " << ch << " " << rhs.get_ch() << endl;
      return *this;
    }

  // how to handle tdc?
  lopost += rhs.get_lopost();
  lopre  += rhs.get_lopre();

  if ( lopost > 4095 ) lopost = 4095;
  if ( lopre > 4095 )  lopre = 4095;

  return *this;
}

mpcRawContent& mpcRawContentV1::operator+(const mpcRawContent &rhs)
{
  mpcRawContentV1 *new_rawcontent = new mpcRawContentV1;
  *new_rawcontent = *this;

  *new_rawcontent += rhs;

  return *new_rawcontent;
}

short mpcRawContentV1::get_ch() const
{
  recoConsts *rc = recoConsts::instance();
  int run_number = rc->get_IntFlag("RUNNUMBER");

  // Should cover run08dAu to end of run10
  if ( run_number > 246200 && run_number < 319000 )
    {
      static int count = 0;
      if ( count < 5 )
        {
          cout << "mpcSimTowerContentV1:get_ch(), looks like run " << run_number
               << " implementing mpc sim channel swap..." << endl;
          count++;
        }

      if ( ch==312 ) return 313;
      else if ( ch==313 ) return 312;
      else if ( ch==96 ) return 261;
      else if ( ch==97 ) return 98;
      else if ( ch==98 ) return 97;
      else if ( ch==99 ) return 96;
      else if ( ch==261 ) return 99;
      else if ( ch==121 ) return 122;
      else if ( ch==123 ) return 120;
      else if ( ch==120 ) return 123;
      else if ( ch==122 ) return 121;
    }

  return ch;
}

void mpcRawContentV1::print(std::ostream& out)
{
  out << ch << "\t" << tdc << "\t" << lopost << "\t" << lopre << endl;
}
