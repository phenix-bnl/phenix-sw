#include <mpcGeaClusterContainerV1.h>
#include <mpcGeaClusterContentV1.h>
#include <TClonesArray.h>

ClassImp(mpcGeaClusterContainerV1)

using namespace std;

static const unsigned int MAXCHANNELS = 20000;

mpcGeaClusterContainerV1::mpcGeaClusterContainerV1()
{
  mpcgclus = new TClonesArray("mpcGeaClusterContentV1",MAXCHANNELS);

  Reset();
}

mpcGeaClusterContainerV1::~mpcGeaClusterContainerV1()
{
  if ( mpcgclus ) delete mpcgclus;
}

/*
int mpcGeaClusterContainerV1::findPrimary(const int feech)
{
  if ( !GetArray() ) return -1;

  float max_edep = 0.;
  int   max_itorigin = -9999;

  for (int ig=0; ig<size(); ig++)
   {
     mpcGeaClusterContent *geaclus = getClus(ig);
     if ( geaclus->get_ch() == feech )
       {
         if ( geaclus->get_edep() > max_edep )
           {
             max_edep = geaclus->get_edep();
             max_itorigin = geaclus->get_itorigin();
           }
       }
   }

  return max_itorigin;
}
*/

mpcGeaClusterContent* mpcGeaClusterContainerV1::addCluster(const mpcGeaClusterContent &clus)
{
  // First check if TC array exists (it does not for the base class
  if (!GetArray())
    {
      return NULL;
    }

  new ((*mpcgclus)[n_mpcgeaclusters]) mpcGeaClusterContentV1( clus );
  mpcGeaClusterContent *temp = (mpcGeaClusterContentV1*)GetArray()->At(n_mpcgeaclusters);

  n_mpcgeaclusters++;

  return temp;
}

void mpcGeaClusterContainerV1::Reset()
{
  GetArray()->Clear();
  n_mpcgeaclusters = 0;
}

//_____________________________________________________________________________
void mpcGeaClusterContainerV1::identify(ostream& os ) const
{
  os << "identify yourself: mpcGeaClusterContainerV1 Object" << endl;
  os << "No of Entries: " << size() << endl;
  return;
}

void mpcGeaClusterContainerV1::print(std::ostream& out) const
{
  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      getCluster(i)->print(out);
    }
}

