
//INCLUDECHECKER: Removed this line: #include "Acc.h"
#include "AccHitv1.h"

ClassImp(AccHitv1)

using std::cout;
using std::endl;

static const unsigned int ACCNHIT = 1000;

AccHitv1::AccHitv1()
{
  nHit = 0;
  AccHits  = new TClonesArray("AccSnglHitv1",ACCNHIT);
}

AccHitv1::AccHitv1(const AccHitv1& rhs)
{
  AccHits = 0;
  rhs.copyto(*this);
}

AccHitv1& AccHitv1::operator=(const AccHitv1& rhs)
{
  if(this != &rhs){
    rhs.copyto(*this);
  }
  return *this;
}

void AccHitv1::copyto(AccHitv1& dest) const
{
  delete dest.AccHits;
  dest.AccHits  = new TClonesArray("AccSnglHitv1",nHit);
  dest.nHit = nHit;
  for(unsigned int i=0;i<nHit;++i){
    AccSnglHitv1* src = static_cast<AccSnglHitv1*>(get_hit(i));
    if( src ){
      dest.AddHit(i);
      AccSnglHitv1* d = static_cast<AccSnglHitv1*>(dest.get_hit(i));
      *d = *src;
    }
    else{
      cout << PHWHERE << "src particle is null ?" << endl;
    }
  }
}

AccHitv1* AccHitv1::clone() const
{
  return new AccHitv1(*this);
}

AccHitv1::~AccHitv1()
{
  if (AccHits)
    {
      AccHits->Clear();
      delete AccHits;
    }
  return ;
}

void AccHitv1::Reset()
{
  AccHits->Clear();
  if(nHit>ACCNHIT){
    AccHits->Expand(ACCNHIT);
  }
  nHit = 0;

  return;
}

int AccHitv1::isValid() const
{
  return ( (nHit>0) ? 1 : 0);
}

void AccHitv1::identify(std::ostream& os) const
{
  os << "identify yourself: AccHitv1 Object " << std::endl;
  os << "No of Hits: " << nHit << std::endl;
   
  return;
}

int AccHitv1::set_TClonesArraySize(const unsigned int nhit)
{
  if(nhit > ACCNHIT){
    AccHits->Expand(nhit);
  }

  return nhit;
}

void AccHitv1::AddHit(const unsigned int ihit)
{
  TClonesArray& hit = *AccHits;
  new(hit[ihit]) AccSnglHitv1();

  return;
}

void AccHitv1::RemoveHit(const unsigned int ihit)
{
  AccHits->RemoveAt(ihit);

  return;
}

AccSnglHitv1* AccHitv1::AddHit(const unsigned int ihit, const AccSnglHit& sngl)
{
  const AccSnglHitv1* hit = dynamic_cast<const AccSnglHitv1*>(&sngl);

  if(!hit){
    cout << PHWHERE << " sngl is not of type AccSnglHitv1" << std::endl;
    return 0;
  }

  return new( (*AccHits)[ihit] ) AccSnglHitv1(*hit);
}

AccSnglHitv1* AccHitv1::get_hit(const unsigned int ihit) const
{
  AccSnglHitv1* sngl = (AccSnglHitv1*) GetHit()->UncheckedAt(ihit);
  return sngl;
}

void AccHitv1::set_boxid(const int ihit, const int val)
{
  AccSnglHit* sngl = (AccSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_boxid(val);
  else{
    std::cout << PHWHERE << "Error no AccHitv1 object found" << std::endl; 
  }

  return;
}

void AccHitv1::set_npe(const int ihit, const float val)
{
  AccSnglHit* sngl = (AccSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_npe(val);
  else{
    std::cout << PHWHERE << "Error no AccHitv1 object found" << std::endl; 
  }

  return;
}

void AccHitv1::set_tof(const int ihit, const float val)
{
  AccSnglHit* sngl = (AccSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_tof(val);
  else{
    std::cout << PHWHERE << "Error no AccHitv1 object found" << std::endl; 
  }

  return;
}

void AccHitv1::set_tdiff(const int ihit, const float val)
{
  AccSnglHit* sngl = (AccSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_tdiff(val);
  else{
    std::cout << PHWHERE << "Error no AccHitv1 object found" << std::endl; 
  }

  return;
}

void AccHitv1::set_xyz(const int ihit, const int ixyz, const float val)
{
  AccSnglHit* sngl = (AccSnglHitv1*) GetHit()->UncheckedAt(ihit);

  if(sngl) sngl->set_xyz(ixyz, val);
  else{
    std::cout << PHWHERE << "Error no AccHitv1 object found" << std::endl; 
  }

  return;
}

int AccHitv1::get_boxid(const int ihit) const
{
  AccSnglHit* sngl = (AccSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_boxid() : -9999 );
}

float AccHitv1::get_npe(const int ihit) const
{
  AccSnglHit* sngl = (AccSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_npe() : -9999 );
}

float AccHitv1::get_tof(const int ihit) const
{
  AccSnglHit* sngl = (AccSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_tof() : -9999 );
}

float AccHitv1::get_tdiff(const int ihit) const
{
  AccSnglHit* sngl = (AccSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_tdiff() : -9999 );
}

float AccHitv1::get_xyz(const int ihit, const int ixyz) const
{
  AccSnglHit* sngl = (AccSnglHitv1*) GetHit()->UncheckedAt(ihit);

  return ( (sngl) ? sngl->get_xyz(ixyz) : -9999 );
}
