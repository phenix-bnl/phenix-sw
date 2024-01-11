#include <Lvl2Outv1.h>

#include <cstring>

using namespace std;

ClassImp(Lvl2Outv1)

Lvl2Outv1::Lvl2Outv1()
{
  setendianism( LitteEndian );
  namelength = 0;
  datalength = 0;
  name = NULL;
  data = NULL;
}

Lvl2Outv1::~Lvl2Outv1()
{
  if (name != NULL) delete [] name;
  if (data != NULL) delete [] data;
}

void Lvl2Outv1::setname(char *src)
{
  if (name != NULL) delete [] name;
  namelength = strlen(src) + 1;
  name = new char[namelength];
  memcpy(name, src, namelength);
}

void Lvl2Outv1::fill(UINT n, PHDWORD *src_ptr) {
  if (data != NULL) delete [] data;
  datalength = n;
  data = new PHDWORD[n];
  memcpy(data, src_ptr, n*sizeof(PHDWORD));
}

void Lvl2Outv1::Reset()
{
  Clear();
}

void Lvl2Outv1::Clear(const Option_t *opt)
{
  namelength = 0;
  datalength = 0;
  delete [] name;
  name = NULL;
  delete [] data;
  data = NULL;
}
