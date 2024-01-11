#include "mpcGeaTowerContainerV1.h"
#include "mpcGeaTowerContentV1.h"
#include "TClonesArray.h"

ClassImp(mpcGeaTowerContainerV1)

using namespace std;

static const unsigned int MAXCHANNELS = 20000;

mpcGeaTowerContainerV1::mpcGeaTowerContainerV1()
{
  mpcgtow = new TClonesArray("mpcGeaTowerContentV1",MAXCHANNELS);

  Reset();
}

mpcGeaTowerContainerV1::~mpcGeaTowerContainerV1()
{
  if ( mpcgtow ) delete mpcgtow;
}

/*
int mpcGeaTowerContainerV1::findPrimary(const int feech)
{
  if ( !GetArray() ) return -1;

  float max_edep = 0.;
  int   max_itorigin = -9999;

  for (int ig=0; ig<size(); ig++)
   {
     mpcGeaTowerContent *geatow = getTower(ig);
     if ( geatow->get_ch() == feech )
       {
         if ( geatow->get_edep() > max_edep )
           {
             max_edep = geatow->get_edep();
             max_itorigin = geatow->get_itorigin();
           }
       }
   }

  return max_itorigin;
}
*/

mpcGeaTowerContent* mpcGeaTowerContainerV1::addTower(const mpcGeaTowerContent &tower)
{
  // First check if TC array exists (it does not for the base class
  if (!GetArray())
    {
      return NULL;
    }

  new ((*mpcgtow)[n_mpcgeatowers]) mpcGeaTowerContentV1( tower );
  mpcGeaTowerContent *temp = (mpcGeaTowerContentV1*)GetArray()->At(n_mpcgeatowers);

  n_mpcgeatowers++;

  return temp;
}

void mpcGeaTowerContainerV1::Reset()
{
  GetArray()->Clear();
  n_mpcgeatowers = 0;
}

//_____________________________________________________________________________
void mpcGeaTowerContainerV1::identify(ostream& os ) const
{
  os << "identify yourself: mpcGeaTowerContainerV1 Object" << endl;
  os << "No of Entries: " << size() << endl;
  return;
}

void mpcGeaTowerContainerV1::print(std::ostream& out) const
{
  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      getTower(i)->print(out);
    }
}

