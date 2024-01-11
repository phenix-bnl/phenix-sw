#include "emcClusterAuxInfoContainerV1.h"
#include "emcClusterAuxInfoV1.h"
#include "TClonesArray.h"
#include <cassert>
#include <iostream>
#include <algorithm>
#include "phool.h"

ClassImp(emcClusterAuxInfoContainerV1);

using namespace std;

const unsigned int emcClusterAuxInfoContainerV1::fgMaxSize = 24768;
const unsigned int emcClusterAuxInfoContainerV1::fgDefaultSize = 1000;

//_____________________________________________________________________________
  emcClusterAuxInfoContainerV1::emcClusterAuxInfoContainerV1() 
{
  cout << __FILE__ << "  " << __LINE__ << "  emcClusterAuxInfoContainerV1" << endl;
  fAuxInfo = 0;
  allocate(emcClusterAuxInfoContainerV1::fgDefaultSize);

}

//_____________________________________________________________________________
emcClusterAuxInfoContainerV1::~emcClusterAuxInfoContainerV1()
{
  delete fAuxInfo;
}

//_____________________________________________________________________________
int emcClusterAuxInfoContainerV1::addInfo(unsigned int i, const float c, const float e, const float x, const float y)
{
  if ( i >= capacity() )
    {
      bool ok = expand_for(i);
      if (!ok) 
	{
	  cerr << PHWHERE << " object is full ?!" << endl;
	  return -1;
	}
    }

  //  cout << __FILE__ << "  " << __LINE__ << "  emcClusterAuxInfoContainerV1" << endl;

  new((*fAuxInfo)[i]) emcClusterAuxInfoV1(c,e,x,y);
  return 0;
}

//_____________________________________________________________________________
void
emcClusterAuxInfoContainerV1::allocate(unsigned int thesize)
{
  delete fAuxInfo;
  fAuxInfo = new TClonesArray("emcClusterAuxInfoV1",thesize);

}

//_____________________________________________________________________________
unsigned int emcClusterAuxInfoContainerV1::capacity(void) const
{ 
  return fAuxInfo->GetSize(); 
}



//_____________________________________________________________________________
bool
emcClusterAuxInfoContainerV1::expand(unsigned int newcap)
{
  if ( newcap <= fgMaxSize ) 
    {
      fAuxInfo->Expand(newcap);
      return true;
    }
  else
    {
      std::cerr << "emcClusterAuxInfoContainerV1::expand : "
		<< " attemting to go above max capacity of "
		<< fgMaxSize << ". That's a *big* failure "
		<< "probably"
		<< std::endl;
      return false;
    }
}

//_____________________________________________________________________________
bool emcClusterAuxInfoContainerV1::expand_for(unsigned int index)
{
  unsigned int capa = capacity();

  while ( index >= capa && capa < fgMaxSize ) 
    {
      capa = std::min(capa*2,fgMaxSize);
    }
  if ( index >= capa )
    {
      return false;
    }
  else
    {
      assert(capa>capacity());
      return expand(capa);
    }
}

//_____________________________________________________________________________
emcClusterAuxInfo* emcClusterAuxInfoContainerV1::getInfo(unsigned int i) const
{
  return static_cast<emcClusterAuxInfoV1*>(fAuxInfo->At(i));
}

//_____________________________________________________________________________
void
emcClusterAuxInfoContainerV1::identify(ostream& os) const
{
  os << "emcClusterAuxInfoContainerV1::identify : size=" << size() << std::endl;
}

//_____________________________________________________________________________
void
emcClusterAuxInfoContainerV1::Reset()
{
  fAuxInfo->Clear();
}

//_____________________________________________________________________________
unsigned int
emcClusterAuxInfoContainerV1::size(void) const
{
  return fAuxInfo->GetLast()+1;
}
