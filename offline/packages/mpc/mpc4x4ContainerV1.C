#include "mpc4x4ContainerV1.h"
#include "mpc4x4ContentV1.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(mpc4x4ContainerV1)

using namespace std;

static const unsigned int MAXCHANNELS = 144;

mpc4x4ContainerV1::mpc4x4ContainerV1()
{
  mpc4x4s = new TClonesArray("mpc4x4ContentV1",MAXCHANNELS);

  Reset();
}

mpc4x4ContainerV1::~mpc4x4ContainerV1()
{
  if ( mpc4x4s ) delete mpc4x4s;
}

mpc4x4Container& mpc4x4ContainerV1::operator=(const mpc4x4Container& rhs)
{
  if ( ((mpc4x4Container*) this) == &rhs ) return *this;

  n_mpc4x4s = rhs.size();

  *mpc4x4s = *(rhs.GetArray());

  return *this;
}

mpc4x4Content* mpc4x4ContainerV1::add4x4(mpc4x4Content &i4x4)
{
  // First check if TC array exists (it does not for the base class
  if (!GetArray())
    {
      return NULL;
    }
  new ((*mpc4x4s)[n_mpc4x4s]) mpc4x4ContentV1( i4x4 );
  mpc4x4Content *temp = (mpc4x4ContentV1*)GetArray()->At(n_mpc4x4s);
  n_mpc4x4s++;
  return temp;
}

void mpc4x4ContainerV1::Reset()
{
  GetArray()->Clear();
  n_mpc4x4s = 0;
}

//_____________________________________________________________________________
void mpc4x4ContainerV1::identify(ostream& os ) const
{
  os << "identify yourself: mpc4x4ContainerV1 Object" << endl;
  os << "No of Entries: " << size() << endl;
  return;
}

void mpc4x4ContainerV1::print(std::ostream& out) const
{
  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      get4x4(i)->print(out);
    }
}

