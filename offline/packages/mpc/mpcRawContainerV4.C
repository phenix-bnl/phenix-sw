#include <mpcRawContainerV4.h>
#include <mpcRawContentV4.h>
#include <TClonesArray.h>

ClassImp(mpcRawContainerV4)

using namespace std;

static const unsigned int MAXCHANNELS = 576;

mpcRawContainerV4::mpcRawContainerV4()
{
  mpcraw = new TClonesArray("mpcRawContentV4",MAXCHANNELS);

  Reset();
}

mpcRawContainerV4::~mpcRawContainerV4()
{
  if ( mpcraw ) delete mpcraw;
}

mpcRawContainer& mpcRawContainerV4::operator=(const mpcRawContainer &rhs)
{
  if ( this == &rhs ) return *this;

  n_mpcraw = rhs.size();

  *mpcraw = *(rhs.GetArray());

  return *this;
}

mpcRawContainer& mpcRawContainerV4::operator+=(const mpcRawContainer &rhs)
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
          mpcRawContentV4 temp_rawtowercontent = *rhs_raw;
          this->addTower( temp_rawtowercontent );
        }
      else if ( found_common_channel>1 )
        {
          cerr << PHWHERE << " multiple common channels?" << endl;
        }
    }

  return *this;
}

mpcRawContainer& mpcRawContainerV4::operator+(const mpcRawContainer &rhs)
{
  mpcRawContainerV4 *new_rawcontainer = new mpcRawContainerV4;
  *new_rawcontainer = *this;

  *new_rawcontainer += rhs;

  return *new_rawcontainer;
}

mpcRawContent* mpcRawContainerV4::addTower(const mpcRawContent &clus)
{
  // First check if TC array exists (it does not for the base class
  if (!GetArray())
    {
      return NULL;
    }

  new ((*mpcraw)[n_mpcraw]) mpcRawContentV4( clus );
  mpcRawContent *temp = (mpcRawContentV4*)GetArray()->At(n_mpcraw);

  n_mpcraw++;

  return temp;
}

void mpcRawContainerV4::Reset()
{
  GetArray()->Clear();
  n_mpcraw = 0;
}

//_____________________________________________________________________________
void mpcRawContainerV4::identify(ostream& os ) const
{
  os << "identify yourself: mpcRawContainerV4 Object" << endl;
  os << "No of Entries: " << size() << endl;
  return;
}

void mpcRawContainerV4::print(std::ostream& out) const
{
  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      getTower(i)->print(out);
    }
}

