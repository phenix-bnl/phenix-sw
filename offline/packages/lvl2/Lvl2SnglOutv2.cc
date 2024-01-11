#include <iostream>
#include <Lvl2SnglOutv2.h>

#include <cstring>

ClassImp(Lvl2SnglOutv2)

Lvl2SnglOutv2::Lvl2SnglOutv2()
{
  //dummy_data[0] = 0;
  //dummy_name[0] = '\0';
  setendianism( LitteEndian );
  datalength = 0;
  data = 0;
}

Lvl2SnglOutv2::~Lvl2SnglOutv2()
{
  if (name.length() > 0) name.erase();

  if (datalength > 0 && data != 0)
    {
      delete [] data;
      data = 0;
      datalength = 0;
    }
}

void Lvl2SnglOutv2::identify(std::ostream& os) const
{
  os << "identify yourself: I am a Lvl2SnglOutv2 object" << std::endl;
}

void Lvl2SnglOutv2::fill(UINT n, PHDWORD *src_ptr)
{
  if (n == 0) return;
  if (datalength > 0 && data != 0) delete [] data;
  datalength = n;
  data = new PHDWORD[n];
  memcpy(data, src_ptr, n*sizeof(PHDWORD));
}

void Lvl2SnglOutv2::Reset()
{
  //  namelength = 0;
  Clear();
}

void Lvl2SnglOutv2::Clear(const Option_t *opt)
{
  if (name.length() > 0) name.erase();

  if (datalength > 0 && data != 0)
    {
      delete [] data;
      datalength = 0;
      data = 0;
    }
}

