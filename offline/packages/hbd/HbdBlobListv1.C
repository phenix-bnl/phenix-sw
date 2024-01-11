#include "HbdBlobListv1.h"
#include "HbdBlobv1.h"
#include <iostream>

ClassImp(HbdBlobListv1)

using namespace std;

#define HBDNBLOB 3000

HbdBlobListv1::HbdBlobListv1()
{
  nBlobs = 0;
  Blob = new TClonesArray("HbdBlobv1",HBDNBLOB);
}

HbdBlobListv1::HbdBlobListv1(const HbdBlobListv1& rhs)
{
  Blob = 0;
  rhs.copyto(*this);
}

HbdBlobListv1& 
HbdBlobListv1::operator=(const HbdBlobListv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
HbdBlobListv1::copyto(HbdBlobListv1& dest) const
{
  delete dest.Blob;
  dest.Blob = new TClonesArray("HbdBlobv1",nBlobs);
  dest.nBlobs = nBlobs;
  for ( unsigned int i = 0; i < nBlobs; ++i ) 
    {
      HbdBlobv1* src = static_cast<HbdBlobv1*>
	(get_blob(i));
      if ( src ) 
	{
	  dest.AddBlob(i);
	  HbdBlobv1* d = static_cast<HbdBlobv1*>
	    (dest.get_blob(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

HbdBlobListv1* 
HbdBlobListv1::clone() const
{
  return new HbdBlobListv1(*this);
}

HbdBlobListv1::~HbdBlobListv1()
{
  Blob->Clear();
  delete Blob;
  return;
}

HbdBlobv1* HbdBlobListv1::get_blob (const unsigned int iblob) const {
  HbdBlobv1 *Particle = (HbdBlobv1 *) GetBlob()->UncheckedAt(iblob);
  return Particle;
}

void HbdBlobListv1::identify(ostream& os) const
{
  os << "identify yourself: HbdBlobListv1 Object\n"
     << "No of Blobs: " << nBlobs << std::endl;
  return;
}

void HbdBlobListv1::Reset()
{
  Blob->Clear();
  if (nBlobs>HBDNBLOB)
    {
      Blob->Expand(HBDNBLOB);
    }
  nBlobs = 0;
  return;
}

int HbdBlobListv1::isValid() const
{
  return((nBlobs>0) ? 1 : 0);
}

int HbdBlobListv1::set_TClonesArraySize(const unsigned int nblob)
{
  if (nblob > HBDNBLOB)
    {
      Blob->Expand(nblob);
    }
  return nblob;
}

void  HbdBlobListv1::AddBlob(const unsigned int iblob)
{
  TClonesArray &Particle = *Blob;
  new(Particle[iblob]) HbdBlobv1();
  return;
}

HbdBlobv1* 
HbdBlobListv1::AddBlob(const unsigned int iblob,
			     const HbdBlob& blobt)
{
  const HbdBlobv1* test = dynamic_cast<const HbdBlobv1*>
    (&blobt);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type HbdBlobv1"
	   << endl;
      return 0;
    }

  return new((*Blob)[iblob]) HbdBlobv1(*test);
}


void  HbdBlobListv1::RemoveBlob(const unsigned int iblob)
{
  Blob->RemoveAt(iblob);
  return;
}

