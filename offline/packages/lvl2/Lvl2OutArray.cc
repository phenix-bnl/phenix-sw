#include <Lvl2OutArray.h>
#include <Lvl2SnglOut.h>
#include <phool.h>

#include <TClonesArray.h>

#include <iostream>

ClassImp(Lvl2OutArray)

using namespace std;

void 
Lvl2OutArray::Reset()
{
  std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
}

int 
Lvl2OutArray::isValid() const
{
  std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
  return 0;
}

void 
Lvl2OutArray::identify(std::ostream &os) const
{
  os << "identify yourself: virtual Lvl2OutArray object" << std::endl;
}

Lvl2SnglOut *
Lvl2OutArray::getPrimitive(const unsigned int iprim) const
{
  if (!GetL2Prim())
    {
      cout << "No L2 Prim TClonesArray, calling identify to show to whom you are talking" << endl;
      identify();
    }
  Lvl2SnglOut *Prim = static_cast<Lvl2SnglOut *> (GetL2Prim()->UncheckedAt(iprim));
 return Prim;
}

const char *
Lvl2OutArray::getname(const unsigned int iprim) const
{
  Lvl2SnglOut *out = getPrimitive(iprim);
  return ((out) ? out->getname() : "");
}


short
Lvl2OutArray::getversion(const unsigned int iprim) const
{
  Lvl2SnglOut *out = getPrimitive(iprim);
  return ((out) ? out->getversion() : -999);
}

void
Lvl2OutArray::setversion(const unsigned int iprim, const short v)
{
  Lvl2SnglOut *out = getPrimitive(iprim);
  if (out) out->setversion(v);
  else     cout << PHWHERE << "ERROR no Lvl2SnglOut object found" << endl;
}

int
Lvl2OutArray::getdatalength(const unsigned int iprim) const
{
  Lvl2SnglOut *out = getPrimitive(iprim);
  return ((out) ? out->getdatalength() : -9999);
}

PHDWORD *
Lvl2OutArray::getdata(const unsigned int iprim) const
{
  Lvl2SnglOut *out = getPrimitive(iprim);
  return ((out) ? out->getdata() : 0);
}

void
Lvl2OutArray::setname(const unsigned int iprim, const char *src_ptr)
{
  Lvl2SnglOut *out = getPrimitive(iprim);
  if (out) out->setname(src_ptr);
  else     cout << PHWHERE << "ERROR no Lvl2SnglOut object found" << endl;
}

short
Lvl2OutArray::getendianism(const unsigned int iprim) const
{
  Lvl2SnglOut *out = getPrimitive(iprim);
  return ((out) ? out->getendianism() : -999);
}

void
Lvl2OutArray::setendianism(const unsigned int iprim, const short e)
{
  Lvl2SnglOut *out = getPrimitive(iprim);
  if (out) out->setendianism(e);
  else     cout << PHWHERE << "ERROR no Lvl2SnglOut object found" << endl;
}
