#include <mpcSampleContainerV1.h>

ClassImp(mpcSampleContainerV1)

using namespace std;

static const unsigned int MAXSAMPLES = 12*576;

mpcSampleContainerV1::mpcSampleContainerV1()
{
  mpcsamples = new TClonesArray("mpcSampleV1",MAXSAMPLES);
  n_samples = 12;
  n_channels = 576;
  n_entries = 0;
  //cout << size() << "\t" << n_samples << "\t" << n_channels << "\t" << n_entries << "\t" << endl;
}

mpcSampleContainerV1::~mpcSampleContainerV1()
{
  if ( mpcsamples ) delete mpcsamples;
}

mpcSample* mpcSampleContainerV1::AddSample(const mpcSample &isamp)
{
  //
  if (!GetArray())
  {
    return NULL;
  }

  new ((*mpcsamples)[n_entries]) mpcSampleV1( isamp );
  mpcSample *temp = (mpcSampleV1*)GetArray()->At(n_entries);

  n_entries++;

  return temp;
}

void mpcSampleContainerV1::Reset()
{
/*
  static int a = 0;
  if ( a<10 )
    {
      cout << "In mpcSampleContainerV1::Reset()" << endl;
      a++;
    }
*/
  GetArray()->Clear();
  n_entries = 0;

  mpcSampleContainer::Reset();
}

void mpcSampleContainerV1::print(std::ostream& os) const
{
  //os << "mpcSampleContainerV1" << endl;
}

