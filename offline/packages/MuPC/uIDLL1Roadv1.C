#include "uIDLL1Roadv1.h"
#include "uIDLL1SnglRoadv1.h"
#include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(uIDLL1Roadv1)

using namespace std;
static const unsigned int MUPCCLUSTER = 100;

  // First we implement the "standard functions"...
uIDLL1Roadv1::uIDLL1Roadv1()
{
  nDeepRoad = 0;
  DeepRoad  = new TClonesArray("uIDLL1SnglRoadv1",MUPCCLUSTER);
}

uIDLL1Roadv1::uIDLL1Roadv1(const uIDLL1Roadv1& rhs)
{
  DeepRoad=0;
  rhs.copyto(*this);
}

uIDLL1Roadv1& 
uIDLL1Roadv1::operator=(const uIDLL1Roadv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
uIDLL1Roadv1::copyto(uIDLL1Roadv1& dest) const
{
  delete dest.DeepRoad;
  dest.DeepRoad = new TClonesArray("uIDLL1SnglRoadv1",nDeepRoad);
  dest.nDeepRoad = nDeepRoad;
  for ( unsigned int i = 0; i < nDeepRoad; ++i ) 
    {
      uIDLL1SnglRoadv1* src = static_cast<uIDLL1SnglRoadv1*>
	(get_deep_road(i));
      if ( src ) 
	{
	  dest.AddDeepRoad(i);
	  uIDLL1SnglRoadv1* d = static_cast<uIDLL1SnglRoadv1*>
	    (dest.get_deep_road(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

uIDLL1Roadv1* 
uIDLL1Roadv1::clone() const
{
  return new uIDLL1Roadv1(*this);
}

uIDLL1Roadv1::~uIDLL1Roadv1()
{
  DeepRoad->Clear();
  return;
}

uIDLL1SnglRoad* 
uIDLL1Roadv1::get_deep_road (const unsigned int itrk) const {
  uIDLL1SnglRoadv1 *Particle = (uIDLL1SnglRoadv1 *) GetDeepRoad()->UncheckedAt(itrk);
  return Particle;
}


void uIDLL1Roadv1::identify(std::ostream& os) const
{
  os << "identify yourself: uIDLL1Roadv1 Object\n"
     << "No of DeepRoad: " << nDeepRoad << std::endl;
  return;
}

void uIDLL1Roadv1::Reset()
{
 DeepRoad->Clear();
 if (nDeepRoad>MUPCCLUSTER)
   {
     DeepRoad->Expand(MUPCCLUSTER);
   }
 nDeepRoad = 0;
 return;
}

int uIDLL1Roadv1::isValid() const
{
  return((nDeepRoad>0) ? 1 : 0);
}

int uIDLL1Roadv1::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > MUPCCLUSTER)
    {
      DeepRoad->Expand(nhits);
     }
  return nhits;
}

void  uIDLL1Roadv1::AddDeepRoad(const unsigned int itrk)
{
  TClonesArray &Particle = *DeepRoad;
  new(Particle[itrk]) uIDLL1SnglRoadv1();
  return;
}

uIDLL1SnglRoad* 
uIDLL1Roadv1::AddDeepRoad(const unsigned int itrk,
				const uIDLL1SnglRoad& track)
{
  const uIDLL1SnglRoadv1* test = dynamic_cast<const uIDLL1SnglRoadv1*>
    (&track);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type uIDLL1SnglRoadv1"
	   << endl;
      return 0;
    }

  return new((*DeepRoad)[itrk]) uIDLL1SnglRoadv1(*test);
}


void  uIDLL1Roadv1::RemoveDeepRoad(const unsigned int itrk)
{
  DeepRoad->RemoveAt(itrk);
  return;
}


