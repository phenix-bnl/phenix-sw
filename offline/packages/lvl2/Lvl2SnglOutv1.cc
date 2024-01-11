#include <iostream>
#include <Lvl2SnglOutv1.h>

ClassImp(Lvl2SnglOutv1)

Lvl2SnglOutv1::Lvl2SnglOutv1()
{
  //dummy_data[0] = 0;
  //dummy_name[0] = '\0';
  setendianism( LitteEndian );
  datalength = 0;
  data = 0;
/*
  name = 0;
*/
}

Lvl2SnglOutv1::~Lvl2SnglOutv1()
{
/*
  if (namelength > 0 && name != 0) delete [] name;
*/
  if (datalength > 0 && data != 0)
    {
      delete [] data;
      data = 0;
      datalength = 0;
    }
}

void Lvl2SnglOutv1::identify(std::ostream& os) const
{
  os << "identify yourself: I am a Lvl2SnglOutv1 object" << std::endl;
}

void Lvl2SnglOutv1::fill(UINT n, PHDWORD *src_ptr)
{
  if (n == 0) return;
  if (datalength > 0 && data != 0) delete [] data;
  datalength = n;
  data = new PHDWORD[n];
  memcpy(data, src_ptr, n*sizeof(PHDWORD));
}

void Lvl2SnglOutv1::Reset()
{
  //  namelength = 0;
  Clear();
}

void Lvl2SnglOutv1::Clear(const Option_t *opt)
{
  if (datalength > 0 && data != 0)
    {
      delete [] data;
      datalength = 0;
      data = 0;
    }
}

