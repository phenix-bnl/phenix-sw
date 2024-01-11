#include "mpcRawContainerV1.h"
#include "mpcRawContentV1.h"
#include "TClonesArray.h"

ClassImp(mpcRawContainerV1)

using namespace std;

static const unsigned int MAXENTRIES = 288;

mpcRawContainerV1::mpcRawContainerV1()
{
  mpcraw = new TClonesArray("mpcRawContentV1",MAXENTRIES);
  n_mpcraw = 0;
}

mpcRawContainerV1::~mpcRawContainerV1()
{
  if ( mpcraw ) delete mpcraw;
}

mpcRawContainer& mpcRawContainerV1::operator=(const mpcRawContainer &rhs)
{
  if ( this == &rhs ) return *this;

  n_mpcraw = rhs.size();

  *mpcraw = *(rhs.GetArray());

  return *this;
}

mpcRawContainer& mpcRawContainerV1::operator+=(const mpcRawContainer &rhs)
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
          mpcRawContentV1 temp_rawtowercontent = *rhs_raw;
          this->addTower( temp_rawtowercontent );
        }
      else if ( found_common_channel>1 )
        {
          cerr << PHWHERE << " multiple common channels?" << endl;
        }
    }

  return *this;
}

mpcRawContainer& mpcRawContainerV1::operator+(const mpcRawContainer &rhs)
{
  mpcRawContainerV1 *new_rawcontainer = new mpcRawContainerV1;
  *new_rawcontainer = *this;

  *new_rawcontainer += rhs;

  return *new_rawcontainer;
}

//_____________________________________________________________________________
void
mpcRawContainerV1::print(std::ostream& out) const
{
  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      getTower(i)->print(out);
    }
}

mpcRawContent*
mpcRawContainerV1::addTower(const mpcRawContent &clus)
{
  // First check if TC array exists (it does not for the base class
  if (!GetArray())
    {
      return NULL;
    }

/*
  // this is a bit ugly but GetLast returns the  index-1, so the argument
  // for the ExpandCreate is  GetLast() + 2
  int nnew;
  nnew = GetArray()->GetLast() + 2;
  // this is a TCArray method, it creates a new Object of
  // the type which is stored in the TCArray. It uses the default ctor
  GetArray()->ExpandCreate(nnew);
  mpcRawContent *newclus = static_cast<mpcRawContent *> (GetArray()->UncheckedAt(GetArray()->GetLast()));
  // Since the ExpandCreate calls the default ctor we still need to copy
  // the actual values from the input particle
  newclus->Copy(clus);
*/
  new ((*mpcraw)[n_mpcraw]) mpcRawContentV1( clus );
  mpcRawContent *temp = (mpcRawContentV1*)GetArray()->At(n_mpcraw);

  n_mpcraw++;

  return temp;
}

void mpcRawContainerV1::Reset()
{
  GetArray()->Clear();
  n_mpcraw = 0;
}

int mpcRawContainerV1::isValid() const
{
  int num_entries = size();
  return((num_entries>0) ? 1 : 0);
}

void mpcRawContainerV1::identify(ostream& os ) const
{
  os << "identify yourself: mpcRawContainerV1 Object" << endl;
  os << "No of Entries: " << size() << endl;
  return;
}

