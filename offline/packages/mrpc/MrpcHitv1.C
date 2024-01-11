#include <MrpcHitv1.h>
#include <iostream>

ClassImp(MrpcHitv1)

using std::cout;
using std::endl;

static const unsigned int MRPCNHIT = 1000;

MrpcHitv1::MrpcHitv1()
{
  nHit = 0;
  MrpcHits  = new TClonesArray("MrpcSnglHitv1",MRPCNHIT);
}

MrpcHitv1::MrpcHitv1(const MrpcHitv1& rhs)
{
  MrpcHits = 0;
  rhs.copyto(*this);
}

MrpcHitv1& MrpcHitv1::operator=(const MrpcHitv1& rhs)
{
  if(this != &rhs)
    {
      rhs.copyto(*this);
    }
  return *this;
}

void MrpcHitv1::copyto(MrpcHitv1& dest) const
{
  delete dest.MrpcHits;
  dest.MrpcHits  = new TClonesArray("MrpcSnglHitv1",nHit);
  dest.nHit = nHit;
  for(unsigned int i=0;i<nHit;++i)
    {
      MrpcSnglHitv1* src = static_cast<MrpcSnglHitv1*>(get_hit(i));
      if( src )
	{
	  dest.AddHit(i);
	  MrpcSnglHitv1* d = static_cast<MrpcSnglHitv1*>(dest.get_hit(i));
	  *d = *src;
	}
      else
	{
	  cout << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

MrpcHitv1* MrpcHitv1::clone() const
{
  return new MrpcHitv1(*this);
}

MrpcHitv1::~MrpcHitv1()
{
  if (MrpcHits)
    {
      MrpcHits->Clear();
      delete MrpcHits;
    }
  return;
}

void MrpcHitv1::Reset()
{
  MrpcHits->Clear();
  if(nHit>MRPCNHIT)
    {
      MrpcHits->Expand(MRPCNHIT);
    }
  nHit = 0;

  return;
}

int MrpcHitv1::isValid() const
{
  return ( (nHit>0) ? 1 : 0);
}

void MrpcHitv1::identify(std::ostream& os) const
{
  os << "identify yourself: MrpcHitv1 Object " << std::endl;
  os << "No of Hits: " << nHit << std::endl;
   
  return;
}

int MrpcHitv1::set_TClonesArraySize(const unsigned int nhit)
{
  if(nhit > MRPCNHIT)
    {
      MrpcHits->Expand(nhit);
    }
  
  return nhit;
}

void MrpcHitv1::AddHit(const unsigned int ihit)
{
  TClonesArray& hit = *MrpcHits;
  new(hit[ihit]) MrpcSnglHitv1();
  
  return;
}

void MrpcHitv1::RemoveHit(const unsigned int ihit)
{
  MrpcHits->RemoveAt(ihit);

  return;
}

MrpcSnglHitv1* MrpcHitv1::AddHit(const unsigned int ihit, const MrpcSnglHit& sngl)
{
  const MrpcSnglHitv1* hit = dynamic_cast<const MrpcSnglHitv1*>(&sngl);

  if(!hit)
    {
      cout << PHWHERE << " sngl is not of type MrpcSnglHitv1" << std::endl;
      return 0;
    }

  return new( (*MrpcHits)[ihit] ) MrpcSnglHitv1(*hit);
}

MrpcSnglHitv1* MrpcHitv1::get_hit(const unsigned int ihit) const
{
  MrpcSnglHitv1* sngl = (MrpcSnglHitv1*) GetHit()->UncheckedAt(ihit);
  return sngl;
}

// ==== set ====
void MrpcHitv1::set_slatid(const int ihit, const int val)
{
  MrpcSnglHit* sngl = (MrpcSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_slatid(val);
  else
    {
      std::cout << PHWHERE << "Error no MrpcHitv1 object found" << std::endl; 
    }
  
  return;
}

void MrpcHitv1::set_time(const int ihit, const float val)
{
  MrpcSnglHit* sngl = (MrpcSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_time(val);
  else
    {
      std::cout << PHWHERE << "Error no MrpcHitv1 object found" << std::endl; 
    }
  
  return;
}

void MrpcHitv1::set_time_dig(const int ihit, const float val)
{
  MrpcSnglHit* sngl = (MrpcSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_time_dig(val);
  else
    {
      std::cout << PHWHERE << "Error no MrpcHitv1 object found" << std::endl; 
    }
  
  return;
}

void MrpcHitv1::set_charge(const int ihit, const float val)
{
  MrpcSnglHit* sngl = (MrpcSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_charge(val);
  else
    {
      std::cout << PHWHERE << "Error no MrpcHitv1 object found" << std::endl; 
    }
  
  return;
}

void MrpcHitv1::set_xyz(const int ihit, const int ixyz, const float val)
{
  MrpcSnglHit* sngl = (MrpcSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_xyz(ixyz, val);
  else
    {
      std::cout << PHWHERE << "Error no MrpcHitv1 object found" << std::endl; 
    }
  
  return;
}

// ==== get ====
int MrpcHitv1::get_slatid(const int ihit) const
{
  MrpcSnglHit* sngl = (MrpcSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_slatid() : -9999 );
}

float MrpcHitv1::get_time(const int ihit) const
{
  MrpcSnglHit* sngl = (MrpcSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_time() : -9999 );
}

float MrpcHitv1::get_time_dig(const int ihit) const
{
  MrpcSnglHit* sngl = (MrpcSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_time_dig() : -9999 );
}

float MrpcHitv1::get_charge(const int ihit) const
{
  MrpcSnglHit* sngl = (MrpcSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_charge() : -9999 );
}

float MrpcHitv1::get_xyz(const int ihit, const int ixyz) const
{
  MrpcSnglHit* sngl = (MrpcSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_xyz(ixyz) : -9999 );
}
