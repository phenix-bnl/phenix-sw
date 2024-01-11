#include <mpcRawContainerV5.h>
#include <mpcRawContentV5.h>
#include <TClonesArray.h>

ClassImp(mpcRawContainerV5)

using namespace std;

static const unsigned int MAXCHANNELS = 576;

mpcRawContainerV5::mpcRawContainerV5()
{
  mpcraw = new TClonesArray("mpcRawContentV5",MAXCHANNELS);

  Reset();
}

mpcRawContainerV5::~mpcRawContainerV5()
{
  if ( mpcraw ) delete mpcraw;
}

mpcRawContainer& mpcRawContainerV5::operator=(const mpcRawContainer &rhs)
{
  if ( this == &rhs ) return *this;

  n_mpcraw = rhs.size();

  *mpcraw = *(rhs.GetArray());

  return *this;
}

mpcRawContainer& mpcRawContainerV5::operator+=(const mpcRawContainer &rhs)
{

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
          mpcRawContentV5 temp_rawtowercontent = *rhs_raw;
          this->addTower( temp_rawtowercontent );
        }
      else if ( found_common_channel>1 )
        {
          cerr << PHWHERE << " multiple common channels?" << endl;
        }
    }

  return *this;
}

mpcRawContainer& mpcRawContainerV5::operator+(const mpcRawContainer &rhs)
{
  mpcRawContainerV5 *new_rawcontainer = new mpcRawContainerV5;
  *new_rawcontainer = *this;

  *new_rawcontainer += rhs;

  return *new_rawcontainer;
}

mpcRawContent* mpcRawContainerV5::addTower(const mpcRawContent &clus)
{
  // First check if TC array exists (it does not for the base class
  if (!GetArray())
    {
      return NULL;
    }

  new ((*mpcraw)[n_mpcraw]) mpcRawContentV5( clus );
  mpcRawContent *temp = (mpcRawContentV5*)GetArray()->At(n_mpcraw);

  n_mpcraw++;

  return temp;
}

void mpcRawContainerV5::Reset()
{
  GetArray()->Clear();
  n_mpcraw = 0;
}

//_____________________________________________________________________________
void mpcRawContainerV5::identify(ostream& os ) const
{
  os << "identify yourself: mpcRawContainerV5 Object" << endl;
  os << "No of Entries: " << size() << endl;
  return;
}

void mpcRawContainerV5::print(std::ostream& out) const
{
  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      getTower(i)->print(out);
    }
}

