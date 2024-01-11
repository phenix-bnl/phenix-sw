#include <mpcRawContentV2.h>
#include <recoConsts.h>

ClassImp(mpcRawContentV2)

using namespace std;

mpcRawContentV2::mpcRawContentV2()
{
  ch     = -9999;
  tdc    = -9999;
  lopost = -9999;
  lopre  = -9999;
  hipost = -9999;
  hipre  = -9999;
}

mpcRawContentV2::mpcRawContentV2(const mpcRawContent &m)
{
  ch     = m.get_ch();
  tdc    = m.get_tdc();
  lopost = m.get_lopost();
  lopre  = m.get_lopre();
  hipost = m.get_hipost();
  hipre  = m.get_hipre();
}


mpcRawContent& mpcRawContentV2::operator=(const mpcRawContent &rhs)
{
  if ( this == &rhs ) return *this;
  
  ch     = rhs.get_ch();
  tdc    = rhs.get_tdc();
  lopost = rhs.get_lopost();
  lopre  = rhs.get_lopre();
  hipost = rhs.get_hipost();
  hipre  = rhs.get_hipre();

  return *this;
}

mpcRawContent& mpcRawContentV2::operator+=(const mpcRawContent &rhs)
{
  if ( ch != rhs.get_ch() )
    {
      cerr << PHWHERE << " ERROR, adding different mpc channels! " << ch << " " << rhs.get_ch() << endl;
      return *this;
    }

  // how to handle tdc?
  lopost += rhs.get_lopost();
  lopre  += rhs.get_lopre();
  hipost += rhs.get_hipost();
  hipre  += rhs.get_hipre();

  if ( lopost > 4095 ) lopost = 4095;
  if ( lopre > 4095 )  lopre = 4095;
  if ( hipost > 4095 ) hipost = 4095;
  if ( hipre > 4095 )  hipre = 4095;

  return *this;
}

mpcRawContent& mpcRawContentV2::operator+(const mpcRawContent &rhs)
{
  mpcRawContentV2 *new_rawcontent = new mpcRawContentV2;
  *new_rawcontent = *this;

  *new_rawcontent += rhs;

  return *new_rawcontent;
}

short mpcRawContentV2::get_ch() const
{
  recoConsts *rc = recoConsts::instance();
  int run_number = rc->get_IntFlag("RUNNUMBER");

  // Should cover run08dAu to end of run10
  if ( run_number > 246200 && run_number < 319000 )
    {
      static int count = 0;
      if ( count < 5 )
        {
          cout << "mpcRawTowerContentV2:get_ch(), looks like run " << run_number
               << " implementing mpc channel swap..." << endl;
          count++;
        }

      if ( ch==312 ) return 313;
      else if ( ch==313 ) return 312;
      else if ( ch==97 ) return 98;
      else if ( ch==99 ) return 96;
      else if ( ch==98 ) return 97;
      else if ( ch==96 ) return 261;
      else if ( ch==261 ) return 99;
      else if ( ch==121 ) return 122;
      else if ( ch==123 ) return 120;
      else if ( ch==120 ) return 123;
      else if ( ch==122 ) return 121;
    }
  else if ( run_number > 319000 && run_number < 380000 )
    {
      // Should cover run11 to end of run12
      // Signal cables on driver 9 were swapped!
      static int count = 0;
      if ( count < 5 )
        {
          cout << "mpcRawTowerContentV2:get_ch(), looks like run " << run_number
               << " implementing mpc channel swap..." << endl;
          count++;
        }

      if ( ch==102 ) return 153;
      else if ( ch==153 ) return 102;
      else if ( ch==124 ) return 130;
      else if ( ch==130 ) return 124;
      else if ( ch==126 ) return 128;
      else if ( ch==128 ) return 126;
      else if ( ch==151 ) return 155;
      else if ( ch==155 ) return 151;
      else if ( ch==150 ) return 154;
      else if ( ch==154 ) return 150;
      else if ( ch==127 ) return 129;
      else if ( ch==129 ) return 127;
      else if ( ch==149 ) return 152;
      else if ( ch==152 ) return 149;
      else if ( ch==148 ) return 177;
      else if ( ch==177 ) return 148;
      else if ( ch==125 ) return 131;
      else if ( ch==131 ) return 125;
      else if ( ch==173 ) return 179;
      else if ( ch==179 ) return 173;

    }

  
  return ch;
}


void mpcRawContentV2::print(std::ostream& out)
{
  out << ch << "\t" << tdc << "\t" << lopost << "\t" << lopre << endl;
}
