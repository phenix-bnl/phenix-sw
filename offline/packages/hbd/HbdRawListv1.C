#include "HbdRawListv1.h"
#include "HbdRawv1.h"
#include <iostream>

ClassImp(HbdRawListv1)

using namespace std;

#define HBDNRAW 3000

HbdRawListv1::HbdRawListv1()
{
  nRaws = 0;
  Raw = new TClonesArray("HbdRawv1",HBDNRAW);
}

HbdRawListv1::HbdRawListv1(const HbdRawListv1& rhs)
{
  Raw = 0;
  rhs.copyto(*this);
}

HbdRawListv1& 
HbdRawListv1::operator=(const HbdRawListv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
HbdRawListv1::copyto(HbdRawListv1& dest) const
{
  delete dest.Raw;
  dest.Raw = new TClonesArray("HbdRawv1",nRaws);
  dest.nRaws = nRaws;
  for ( unsigned int i = 0; i < nRaws; ++i ) 
    {
      HbdRawv1* src = static_cast<HbdRawv1*>
	(get_raw(i));
      if ( src ) 
	{
	  dest.AddRaw(i);
	  HbdRawv1* d = static_cast<HbdRawv1*>
	    (dest.get_raw(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

HbdRawListv1* 
HbdRawListv1::clone() const
{
  return new HbdRawListv1(*this);
}

HbdRawListv1::~HbdRawListv1()
{
  Raw->Clear();
  delete Raw;
  return;
}

HbdRawv1* HbdRawListv1::get_raw (const unsigned int iraw) const {
  HbdRawv1 *Particle = (HbdRawv1 *) GetRaw()->UncheckedAt(iraw);
  return Particle;
}

void HbdRawListv1::identify(ostream& os) const
{
  os << "identify yourself: HbdRawListv1 Object\n"
     << "No of Raws: " << nRaws << std::endl;
  return;
}

void HbdRawListv1::Reset()
{
  Raw->Clear();
  if (nRaws>HBDNRAW)
    {
      Raw->Expand(HBDNRAW);
    }
  nRaws = 0;
  return;
}

int HbdRawListv1::isValid() const
{
  return((nRaws>0) ? 1 : 0);
}

int HbdRawListv1::set_TClonesArraySize(const unsigned int nraw)
{
  if (nraw > HBDNRAW)
    {
      Raw->Expand(nraw);
    }
  return nraw;
}

void  HbdRawListv1::AddRaw(const unsigned int iraw)
{
  TClonesArray &Particle = *Raw;
  new(Particle[iraw]) HbdRawv1();
  return;
}

HbdRawv1* 
HbdRawListv1::AddRaw(const unsigned int iraw,
			     const HbdRaw& rawt)
{
  const HbdRawv1* test = dynamic_cast<const HbdRawv1*>
    (&rawt);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type HbdRawv1"
	   << endl;
      return 0;
    }

  return new((*Raw)[iraw]) HbdRawv1(*test);
}


void  HbdRawListv1::RemoveRaw(const unsigned int iraw)
{
  Raw->RemoveAt(iraw);
  return;
}

