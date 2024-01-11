#include "mpcTowerContainerV1.h"
#include "mpcTowerContentV1.h"
#include "TClonesArray.h"

ClassImp(mpcTowerContainerV1)

using namespace std;

static const unsigned int MAXCHANNELS = 576;

mpcTowerContainerV1::mpcTowerContainerV1()
{
  mpctowers = new TClonesArray("mpcTowerContentV1",MAXCHANNELS);

  Reset();
}

mpcTowerContainerV1::~mpcTowerContainerV1()
{
  if ( mpctowers ) delete mpctowers;
}

mpcTowerContainer& mpcTowerContainerV1::operator=(const mpcTowerContainer& rhs)
{
  if ( this == &rhs ) return *this;

  n_mpctowers = rhs.size();
  mpc_tdc_amu = rhs.get_tdc_amu();
  mpc_pre_amu = rhs.get_pre_amu();
  mpc_post_amu = rhs.get_post_amu();

  *mpctowers = *(rhs.GetArray());

  return *this;
}

mpcTowerContainer& mpcTowerContainerV1::operator+=(const mpcTowerContainer& rhs)
{

/*
  // We just use the lhs amu
  mpc_tdc_amu = rhs.get_tdc_amu();
  mpc_pre_amu = rhs.get_pre_amu();
  mpc_post_amu = rhs.get_post_amu();
*/

  int num_rhs = rhs.size();
  int num_lhs = this->size();

  for (int irhs=0; irhs<num_rhs; irhs++)
    {
      mpcTowerContent *rhs_tow = rhs.getTower( irhs );
      int right_ch = rhs_tow->get_ch();

      int found_common_channel = 0;

      for (int ilhs=0; ilhs<num_lhs; ilhs++)
        {
          mpcTowerContent *lhs_tow = this->getTower(ilhs);
          int left_ch = lhs_tow->get_ch();

          if ( right_ch == left_ch )
            {
              *lhs_tow += (*rhs_tow);
              found_common_channel++;
            }
        }

      // channel wasn't found, add to lhs array
      if ( found_common_channel==0 )
        {
          // Add another entry to the rhs array
          mpcTowerContentV1 temp_towercontent = *rhs_tow;
          this->addTower( temp_towercontent );
        }
      else if ( found_common_channel>1 )
        {
          cerr << PHWHERE << " multiple common channels?" << endl;
        }
    }

  return *this;
}

mpcTowerContainer& mpcTowerContainerV1::operator*=(const float scale)
{

/*
  // We just use the lhs amu
  mpc_tdc_amu = rhs.get_tdc_amu();
  mpc_pre_amu = rhs.get_pre_amu();
  mpc_post_amu = rhs.get_post_amu();
*/

  int num_lhs = this->size();

  for (int ilhs=0; ilhs<num_lhs; ilhs++)
    {
      mpcTowerContent *lhs_tow = this->getTower(ilhs);
      float e = lhs_tow->get_energy();
      
      lhs_tow->set_energy(e*scale);
      // cout << "oldenergy, new energy" << e << ", " << lhs_tow->get_energy() << endl;
    }

  return *this;
}



mpcTowerContainer& mpcTowerContainerV1::operator+(const mpcTowerContainer &rhs)
{
  mpcTowerContainerV1 *new_towercontainer = new mpcTowerContainerV1;
  *new_towercontainer = *this;

  *new_towercontainer += rhs;

  return *new_towercontainer;
}

float mpcTowerContainerV1::get_esum() const
{ 
  float esum = 0.;
  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      esum += getTower(i)->get_energy();
    }

  return esum;
}

mpcTowerContent* mpcTowerContainerV1::addTower(mpcTowerContent &tower)
{
  // First check if TC array exists (it does not for the base class
  if (!GetArray())
    {
      return NULL;
    }

  new ((*mpctowers)[n_mpctowers]) mpcTowerContentV1( tower );
  mpcTowerContent *temp = (mpcTowerContentV1*)GetArray()->At(n_mpctowers);

  n_mpctowers++;

  return temp;
}

void mpcTowerContainerV1::Reset()
{
  GetArray()->Clear();
  n_mpctowers = 0;
  mpc_tdc_amu = -999;
  mpc_pre_amu = -999;
  mpc_post_amu = -999;
}

//_____________________________________________________________________________
void mpcTowerContainerV1::identify(ostream& os ) const
{
  os << "identify yourself: mpcTowerContainerV1 Object" << endl;
  os << "No of Entries: " << size() << endl;
  return;
}

void mpcTowerContainerV1::print(std::ostream& out) const
{
  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      getTower(i)->print(out);
    }
}

