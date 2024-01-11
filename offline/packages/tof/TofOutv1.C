#include "TofOutv1.h"
#include "TofHitv1.h"
//INCLUDECHECKER: Removed this line: #include "dTofReconstructedWrapper.h"

ClassImp(TofOutv1);

using namespace std;

#define NTOFHITS 100

TofOutv1::TofOutv1()
{
  TofNHit = 0;
  TofHits = new TClonesArray("TofHitv1", NTOFHITS);
  return ;
}

TofOutv1::~TofOutv1()
{
  TofHits->Clear();
  return ;
}

void TofOutv1::identify(ostream& os) const
{
  os << "identify yourself: TofOutv1 Object" << endl;
  os << "No of hits: " << TofNHit << endl;
  return ;
}

int TofOutv1::isValid() const
{
  return ((TofNHit > 0) ? 1 : 0);
}

void TofOutv1::Reset()
{
  TofHits->Clear();
  if (TofNHit > NTOFHITS)
    {
      TofHits->Expand(NTOFHITS);
    }
  TofNHit = 0;
  return ;
}

unsigned int TofOutv1::set_TClonesArraySize(const unsigned int nhit)
{
  if (TofNHit > NTOFHITS)
    {
      TofHits->Expand(nhit);
    }
  return nhit;
}

void TofOutv1::AddTofHit(const unsigned int ihit)
{
  TClonesArray &tofhit = *TofHits;
  new(tofhit[ihit]) TofHitv1();
  return ;
}


short int TofOutv1::get_id(const unsigned int ihit) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_id() : -9999);
}

void TofOutv1::set_id(const unsigned int ihit, const short ival)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_id(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}

short int TofOutv1::get_panel(const unsigned int ihit) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_panel() : -9999);
}

void TofOutv1::set_panel(const unsigned int ihit, const short ival)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_panel(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}

short int TofOutv1::get_sector(const unsigned int ihit) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_sector() : -9999);
}

void TofOutv1::set_sector(const unsigned int ihit, const short ival)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_sector(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}

short int TofOutv1::get_side(const unsigned int ihit) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_side() : -9999);
}

void TofOutv1::set_side(const unsigned int ihit, const short ival)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_side(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}

short int TofOutv1::get_slat(const unsigned int ihit) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_slat() : -9999);
}

void TofOutv1::set_slat(const unsigned int ihit, const short ival)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_slat(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}

short int TofOutv1::get_slatid(const unsigned int ihit) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_slatid() : -9999);
}

void TofOutv1::set_slatid(const unsigned int ihit, const short ival)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_slatid(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}


short int TofOutv1::get_qvc(const unsigned int ihit, const short i) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_qvc(i) : -9999);
}

void TofOutv1::set_qvc(const unsigned int ihit, const short i, const short ival)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_qvc(ival, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}

short int TofOutv1::get_tvc(const unsigned int ihit, const short i) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_tvc(i) : -9999);
}

void TofOutv1::set_tvc(const unsigned int ihit, const short i, const short ival)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_tvc(ival, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}

float TofOutv1::get_eloss(const unsigned int ihit) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_eloss() : -9999.9);
}

void TofOutv1::set_eloss(const unsigned int ihit, const float rval)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_eloss(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}

float TofOutv1::get_eloss_err(const unsigned int ihit) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_eloss_err() : -9999.9);
}

void TofOutv1::set_eloss_err(const unsigned int ihit, const float rval)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_eloss_err(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}

float TofOutv1::get_tof(const unsigned int ihit) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_tof() : -9999.9);
}

void TofOutv1::set_tof(const unsigned int ihit, const float rval)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_tof(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}

float TofOutv1::get_tof_err(const unsigned int ihit) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_tof_err() : -9999.9);
}

void TofOutv1::set_tof_err(const unsigned int ihit, const float rval)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_tof_err(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}


float TofOutv1::get_xtof(const unsigned int ihit, const short int ival) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_xtof(ival) : -9999.9);
}

void TofOutv1::set_xtof(const unsigned int ihit, const short i, const float rval)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_xtof(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}

float TofOutv1::get_xtof_err(const unsigned int ihit, const short int ival) const
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_xtof_err(ival) : -9999.9);
  }

void TofOutv1::set_xtof_err(const unsigned int ihit, const short i, const float rval)
{
  TofHitv1 *tofhit = (TofHitv1 *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_xtof_err(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHitv1 object found" << endl;
    }
  return ;
}
