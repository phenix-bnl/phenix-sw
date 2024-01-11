#include "mpcRawContainerV2.h"
#include "mpcRawContentV2.h"
#include "TClonesArray.h"

ClassImp(mpcRawContainerV2)

using namespace std;

static const unsigned int MAXCHANNELS = 576;

mpcRawContainerV2::mpcRawContainerV2()
{
  mpcraw = new TClonesArray("mpcRawContentV2",MAXCHANNELS);

  Reset();
}

mpcRawContainerV2::~mpcRawContainerV2()
{
  if ( mpcraw ) delete mpcraw;
}

mpcRawContainer& mpcRawContainerV2::operator=(const mpcRawContainer &rhs)
{
  if ( this == &rhs ) return *this;

  n_mpcraw = rhs.size();
  mpc_tdc_amu = rhs.get_tdc_amu();
  mpc_pre_amu = rhs.get_pre_amu();
  mpc_post_amu = rhs.get_post_amu();

  *mpcraw = *(rhs.GetArray());

  return *this;
}

mpcRawContainer& mpcRawContainerV2::operator+=(const mpcRawContainer &rhs)
{

/*
  // here we should eventually only use the pedestal subtracted ADC's
  // or implement multiple pedestals
  // in the case of simulation this doesn't matter
  mpc_tdc_amu = rhs.get_tdc_amu();
  mpc_pre_amu = rhs.get_pre_amu();
  mpc_post_amu = rhs.get_post_amu();
*/

  //TClonesArray *rhs_mpcraw = rhs.GetArray();
  int num_rhs = rhs.size();
  int num_lhs = this->size();

  for (int irhs=0; irhs<num_rhs; irhs++)
    {
      mpcRawContent *rhs_raw = rhs.getTower( irhs );
      int right_ch = rhs_raw->get_ch();

      int found_common_channel = 0;

      for (int ilhs=0; ilhs<num_lhs; ilhs++)
        {
          mpcRawContent *lhs_raw = this->getTower(ilhs);
          int left_ch = lhs_raw->get_ch();

          if ( right_ch == left_ch )
            {
              *lhs_raw += (*rhs_raw);
              found_common_channel++;
            }
        }

      // channel wasn't found, add to lhs array
      if ( found_common_channel==0 )
        {
          // Add another entry to the rhs array
          mpcRawContentV2 temp_rawtowercontent = *rhs_raw;
          this->addTower( temp_rawtowercontent );
        }
      else if ( found_common_channel>1 )
        {
          cerr << PHWHERE << " multiple common channels?" << endl;
        }
    }

  return *this;
}

mpcRawContainer& mpcRawContainerV2::operator+(const mpcRawContainer &rhs)
{
  mpcRawContainerV2 *new_rawcontainer = new mpcRawContainerV2;
  *new_rawcontainer = *this;

  *new_rawcontainer += rhs;

  return *new_rawcontainer;
}

mpcRawContent* mpcRawContainerV2::addTower(const mpcRawContent &clus)
{
  // First check if TC array exists (it does not for the base class
  if (!GetArray())
    {
      return NULL;
    }

  new ((*mpcraw)[n_mpcraw]) mpcRawContentV2( clus );
  mpcRawContent *temp = (mpcRawContentV2*)GetArray()->At(n_mpcraw);

  n_mpcraw++;

  return temp;
}

void mpcRawContainerV2::Reset()
{
  GetArray()->Clear();
  n_mpcraw = 0;
  mpc_tdc_amu = -999;
  mpc_pre_amu = -999;
  mpc_post_amu = -999;
}

//_____________________________________________________________________________
void mpcRawContainerV2::identify(ostream& os ) const
{
  os << "identify yourself: mpcRawContainerV2 Object" << endl;
  os << "No of Entries: " << size() << endl;
  return;
}

void mpcRawContainerV2::print(std::ostream& out) const
{
  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      getTower(i)->print(out);
    }
}

