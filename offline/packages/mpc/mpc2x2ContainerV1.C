#include "mpc2x2ContainerV1.h"
#include "mpc2x2ContentV1.h"
#include "TClonesArray.h"

ClassImp(mpc2x2ContainerV1)

using namespace std;

static const unsigned int MAXCHANNELS = 144;

mpc2x2ContainerV1::mpc2x2ContainerV1()
{
  mpc2x2s = new TClonesArray("mpc2x2ContentV1",MAXCHANNELS);

  Reset();
}

mpc2x2ContainerV1::~mpc2x2ContainerV1()
{
  if ( mpc2x2s ) delete mpc2x2s;
}

mpc2x2Container& mpc2x2ContainerV1::operator=(const mpc2x2Container& rhs)
{
  if ( ((mpc2x2Container*) this) == &rhs ) return *this;

  n_mpc2x2s = rhs.size();

  *mpc2x2s = *(rhs.GetArray());

  return *this;
}

mpc2x2Content* mpc2x2ContainerV1::add2x2(mpc2x2Content &i2x2)
{
  // First check if TC array exists (it does not for the base class
  if (!GetArray())
    {
      return NULL;
    }

  new ((*mpc2x2s)[n_mpc2x2s]) mpc2x2ContentV1( i2x2 );
  mpc2x2Content *temp = (mpc2x2ContentV1*)GetArray()->At(n_mpc2x2s);

  n_mpc2x2s++;

  return temp;
}

void mpc2x2ContainerV1::Reset()
{
  GetArray()->Clear();
  n_mpc2x2s = 0;
}

//_____________________________________________________________________________
void mpc2x2ContainerV1::identify(ostream& os ) const
{
  os << "identify yourself: mpc2x2ContainerV1 Object" << endl;
  os << "No of Entries: " << size() << endl;
  return;
}

void mpc2x2ContainerV1::print(std::ostream& out) const
{
  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      get2x2(i)->print(out);
    }
}

