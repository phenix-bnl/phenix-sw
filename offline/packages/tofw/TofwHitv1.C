#include "TofwHitv1.h"

ClassImp(TofwHitv1)

using std::cout;
using std::endl;

static const unsigned int TOFWNHIT = 1000;

TofwHitv1::TofwHitv1()
{
  nHit = 0;
  TofwHits  = new TClonesArray("TofwSnglHitv1",TOFWNHIT);
}

TofwHitv1::TofwHitv1(const TofwHitv1& rhs)
{
  TofwHits = 0;
  rhs.copyto(*this);
}

TofwHitv1& TofwHitv1::operator=(const TofwHitv1& rhs)
{
  if(this != &rhs)
    {
      rhs.copyto(*this);
    }
  return *this;
}

void TofwHitv1::copyto(TofwHitv1& dest) const
{
  delete dest.TofwHits;
  dest.TofwHits  = new TClonesArray("TofwSnglHitv1",nHit);
  dest.nHit = nHit;
  for(unsigned int i=0;i<nHit;++i)
    {
      TofwSnglHitv1* src = static_cast<TofwSnglHitv1*>(get_hit(i));
      if( src )
	{
	  dest.AddHit(i);
	  TofwSnglHitv1* d = static_cast<TofwSnglHitv1*>(dest.get_hit(i));
	  *d = *src;
	}
      else
	{
	  cout << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

TofwHitv1* TofwHitv1::clone() const
{
  return new TofwHitv1(*this);
}

TofwHitv1::~TofwHitv1()
{
  if (TofwHits)
    {
      TofwHits->Clear();
      delete TofwHits;
    }
  return;
}

void TofwHitv1::Reset()
{
  TofwHits->Clear();
  if(nHit>TOFWNHIT)
    {
      TofwHits->Expand(TOFWNHIT);
    }
  nHit = 0;

  return;
}

int TofwHitv1::isValid() const
{
  return ( (nHit>0) ? 1 : 0);
}

void TofwHitv1::identify(std::ostream& os) const
{
  os << "identify yourself: TofwHitv1 Object " << std::endl;
  os << "No of Hits: " << nHit << std::endl;
   
  return;
}

int TofwHitv1::set_TClonesArraySize(const unsigned int nhit)
{
  if(nhit > TOFWNHIT)
    {
      TofwHits->Expand(nhit);
    }
  
  return nhit;
}

void TofwHitv1::AddHit(const unsigned int ihit)
{
  TClonesArray& hit = *TofwHits;
  new(hit[ihit]) TofwSnglHitv1();
  
  return;
}

void TofwHitv1::RemoveHit(const unsigned int ihit)
{
  TofwHits->RemoveAt(ihit);

  return;
}

TofwSnglHitv1* TofwHitv1::AddHit(const unsigned int ihit, const TofwSnglHit& sngl)
{
  const TofwSnglHitv1* hit = dynamic_cast<const TofwSnglHitv1*>(&sngl);

  if(!hit)
    {
      cout << PHWHERE << " sngl is not of type TofwSnglHitv1" << std::endl;
      return 0;
    }

  return new( (*TofwHits)[ihit] ) TofwSnglHitv1(*hit);
}

TofwSnglHitv1* TofwHitv1::get_hit(const unsigned int ihit) const
{
  TofwSnglHitv1* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);
  return sngl;
}

// ==== set ====
void TofwHitv1::set_boxid(const int ihit, const int val)
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_boxid(val);
  else
    {
      std::cout << PHWHERE << "Error no TofwHitv1 object found" << std::endl; 
    }
  
  return;
}

void TofwHitv1::set_chamberid(const int ihit, const int val)
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_chamberid(val);
  else
    {
      std::cout << PHWHERE << "Error no TofwHitv1 object found" << std::endl; 
    }
  
  return;
}

void TofwHitv1::set_nstrip(const int ihit, const int val)
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_nstrip(val);
  else
    {
      std::cout << PHWHERE << "Error no TofwHitv1 object found" << std::endl; 
    }
  return;
}

void TofwHitv1::set_max(const int ihit, const int istrip)
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_max(istrip);
  else
    {
      std::cout << PHWHERE << "Error no TofwHitv1 object found" << std::endl; 
    }
  
  return;
}
void TofwHitv1::set_stripid(const int ihit, const int istrip, const int val)
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_stripid(istrip,val);
  else
    {
      std::cout << PHWHERE << "Error no TofwHitv1 object found" << std::endl; 
    }
  
  return;
}


void TofwHitv1::set_time(const int ihit, const int istrip, const float val)
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_time(istrip,val);
  else
    {
      std::cout << PHWHERE << "Error no TofwHitv1 object found" << std::endl; 
    }
  
  return;
}

void TofwHitv1::set_charge(const int ihit, const int istrip, const float val)
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_charge(istrip, val);
  else
    {
      std::cout << PHWHERE << "Error no TofwHitv1 object found" << std::endl; 
    }
  
  return;
}

void TofwHitv1::set_rawadc(const int ihit, const int istrip, const int irawadc, const float val)
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_rawadc(istrip, irawadc, val);
  else
    {
      std::cout << PHWHERE << "Error no TofwHitv1 object found" << std::endl; 
    }
  
  return;
}

void TofwHitv1::set_rawtdc(const int ihit, const int istrip, const int irawtdc, const float val)
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_rawtdc(istrip, irawtdc, val);
  else
    {
      std::cout << PHWHERE << "Error no TofwHitv1 object found" << std::endl; 
    }
  
  return;
}


void TofwHitv1::set_xyz(const int ihit, const int istrip, const int ixyz, const float val)
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_xyz(istrip, ixyz, val);
  else
    {
      std::cout << PHWHERE << "Error no TofwHitv1 object found" << std::endl; 
    }
  
  return;
}


// ==== get ====

int TofwHitv1::get_boxid(const int ihit) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_boxid() : -9999 );
}

int TofwHitv1::get_chamberid(const int ihit) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_chamberid() : -9999 );
}

int TofwHitv1::get_nstrip(const int ihit) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_nstrip() : -9999 );
}

int TofwHitv1::get_max(const int ihit) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_max() : -9999 );
}

int TofwHitv1::get_stripid(const int ihit, const int istrip) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_stripid(istrip) : -9999 );
}

int TofwHitv1::get_stripid(const int ihit) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  int istrip = get_max(ihit);
  
  return ( (sngl) ? sngl->get_boxid()*32*4+sngl->get_chamberid()*4+sngl->get_stripid(istrip) : -9999 );
}

float TofwHitv1::get_time(const int ihit, const int istrip) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_time(istrip) : -9999 );
}

float TofwHitv1::get_time(const int ihit) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  int istrip = get_max(ihit);

  return ( (sngl) ? sngl->get_time(istrip) : -9999 );
}

float TofwHitv1::get_charge(const int ihit, const int istrip) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_charge(istrip) : -9999 );
}

float TofwHitv1::get_charge(const int ihit) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  int istrip = get_max(ihit);

  return ( (sngl) ? sngl->get_charge(istrip) : -9999 );
}

float TofwHitv1::get_rawadc(const int ihit, const int istrip, const int irawadc) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_rawadc(istrip, irawadc) : -9999 );
}

float TofwHitv1::get_rawadc(const int ihit, const int irawadc) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  int istrip = get_max(ihit);

  return ( (sngl) ? sngl->get_rawadc(istrip, irawadc) : -9999 );
}

float TofwHitv1::get_rawtdc(const int ihit, const int istrip, const int irawtdc) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_rawtdc(istrip, irawtdc) : -9999 );
}

float TofwHitv1::get_rawtdc(const int ihit, const int irawtdc) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  int istrip = get_max(ihit);

  return ( (sngl) ? sngl->get_rawtdc(istrip, irawtdc) : -9999 );
}

float TofwHitv1::get_xyz(const int ihit, const int istrip, const int ixyz) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_xyz(istrip, ixyz) : -9999 );
}

float TofwHitv1::get_xyz(const int ihit, const int ixyz) const
{
  TofwSnglHit* sngl = (TofwSnglHitv1*) GetHit()->UncheckedAt(ihit);

  int istrip = get_max(ihit);

  return ( (sngl) ? sngl->get_xyz(istrip, ixyz) : -9999 );
}
