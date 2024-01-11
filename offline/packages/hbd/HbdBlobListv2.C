#include "HbdBlobListv2.h"
#include "HbdBlobv2.h"
#include <iostream>

ClassImp(HbdBlobListv2)

using namespace std;

#define HBDNBLOB 3000

HbdBlobListv2::HbdBlobListv2()
{
  nBlobs = 0;
  Blob = new TClonesArray("HbdBlobv2",HBDNBLOB);
}

HbdBlobListv2::HbdBlobListv2(const HbdBlobListv2& rhs)
{
  Blob = 0;
  rhs.copyto(*this);
}

HbdBlobListv2& 
HbdBlobListv2::operator=(const HbdBlobListv2& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
HbdBlobListv2::copyto(HbdBlobListv2& dest) const
{
  delete dest.Blob;
  dest.Blob = new TClonesArray("HbdBlobv2",nBlobs);
  dest.nBlobs = nBlobs;
  for ( unsigned int i = 0; i < nBlobs; ++i ) 
    {
      HbdBlobv2* src = static_cast<HbdBlobv2*>
	(get_blob(i));
      if ( src ) 
	{
	  dest.AddBlob(i);
	  HbdBlobv2* d = static_cast<HbdBlobv2*>
	    (dest.get_blob(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

HbdBlobListv2* 
HbdBlobListv2::clone() const
{
  return new HbdBlobListv2(*this);
}

HbdBlobListv2::~HbdBlobListv2()
{
  Blob->Clear();
  delete Blob;
  return;
}

HbdBlobv2* HbdBlobListv2::get_blob (const unsigned int iblob) const {
  HbdBlobv2 *Particle = (HbdBlobv2 *) GetBlob()->UncheckedAt(iblob);
  return Particle;
}

void HbdBlobListv2::identify(ostream& os) const
{
  os << "identify yourself: HbdBlobListv2 Object\n"
     << "No of Blobs: " << nBlobs << std::endl;
  return;
}

void HbdBlobListv2::Reset()
{
  Blob->Clear();
  if (nBlobs>HBDNBLOB)
    {
      Blob->Expand(HBDNBLOB);
    }
  nBlobs = 0;
  return;
}

int HbdBlobListv2::isValid() const
{
  return((nBlobs>0) ? 2 : 0);
}

int HbdBlobListv2::set_TClonesArraySize(const unsigned int nblob)
{
  if (nblob > HBDNBLOB)
    {
      Blob->Expand(nblob);
    }
  return nblob;
}

void  HbdBlobListv2::AddBlob(const unsigned int iblob)
{
  TClonesArray &Particle = *Blob;
  new(Particle[iblob]) HbdBlobv2();
  return;
}

HbdBlobv2* 
HbdBlobListv2::AddBlob(const unsigned int iblob,
			     const HbdBlob& blobt)
{
  const HbdBlobv2* test = dynamic_cast<const HbdBlobv2*>
    (&blobt);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type HbdBlobv2"
	   << endl;
      return 0;
    }

  return new((*Blob)[iblob]) HbdBlobv2(*test);
}


void  HbdBlobListv2::RemoveBlob(const unsigned int iblob)
{
  Blob->RemoveAt(iblob);
  return;
}

