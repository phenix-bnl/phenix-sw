#include <cmath>
#include <iostream>

#include "TClonesArray.h"

#include "TofOutv2.h"
#include "TofHitv2.h"

ClassImp(TofOutv2);

using namespace std;

static unsigned int NTOFHITS = 100;

TofOutv2::TofOutv2()
{
  TofNHit = 0;
  TofHits = new TClonesArray("TofHitv2", NTOFHITS);
  return ;
}

TofOutv2::~TofOutv2()
{
  if (TofHits)
    {
      TofHits->Clear();
      delete TofHits;
    }
  return ;
}

void TofOutv2::identify(ostream& os) const
{
  unsigned int ihit;
  os << "identify yourself: TofOutv2 Object" << endl;
  os << "No of hits: " << TofNHit << endl;
  for(ihit = 0; ihit< TofNHit;ihit++)
    {
      os << "Hit No: " << ihit << endl;
      TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
      tofhit->identify(os);
      os << endl;
    }

  return ;
}

int TofOutv2::isValid() const
{
  return ((TofNHit > 0) ? 1 : 0);
}

void TofOutv2::Reset()
{
  TofHits->Clear();
  if (TofNHit > NTOFHITS)
    {
      TofHits->Expand(NTOFHITS);
    }
  TofNHit = 0;
  return ;
}


unsigned int TofOutv2::set_TClonesArraySize(const unsigned int nhit)
{
  if (TofNHit > NTOFHITS)
    {
      TofHits->Expand(nhit);
    }
  return nhit;
}

void TofOutv2::AddTofHit(const unsigned int ihit)
{
  TClonesArray &tofhit = *TofHits;
  new(tofhit[ihit]) TofHitv2();
  return ;
}


short int TofOutv2::get_id(const unsigned int ihit) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_id() : -9999);
}

void TofOutv2::set_id(const unsigned int ihit, const short ival)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_id(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}

short int TofOutv2::get_panel(const unsigned int ihit) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_panel() : -9999);
}

void TofOutv2::set_panel(const unsigned int ihit, const short ival)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_panel(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}

short int TofOutv2::get_sector(const unsigned int ihit) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_sector() : -9999);
}

void TofOutv2::set_sector(const unsigned int ihit, const short ival)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_sector(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}

short int TofOutv2::get_side(const unsigned int ihit) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_side() : -9999);
}

void TofOutv2::set_side(const unsigned int ihit, const short ival)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_side(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}

short int TofOutv2::get_slat(const unsigned int ihit) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_slat() : -9999);
}

void TofOutv2::set_slat(const unsigned int ihit, const short ival)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_slat(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}

short int TofOutv2::get_slatid(const unsigned int ihit) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_slatid() : -9999);
}

void TofOutv2::set_slatid(const unsigned int ihit, const short ival)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_slatid(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}


short int TofOutv2::get_qvc(const unsigned int ihit, const short i) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_qvc(i) : -9999);
}

void TofOutv2::set_qvc(const unsigned int ihit, const short i, const short ival)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_qvc(ival, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}

short int TofOutv2::get_tvc(const unsigned int ihit, const short i) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_tvc(i) : -9999);
}

void TofOutv2::set_tvc(const unsigned int ihit, const short i, const short ival)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_tvc(ival, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}

float TofOutv2::get_eloss(const unsigned int ihit) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_eloss() : NAN);
}

void TofOutv2::set_eloss(const unsigned int ihit, const float rval)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_eloss(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}

float TofOutv2::get_eloss_err(const unsigned int ihit) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_eloss_err() : NAN);
}

void TofOutv2::set_eloss_err(const unsigned int ihit, const float rval)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_eloss_err(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}

float TofOutv2::get_tof(const unsigned int ihit) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_tof() : NAN);
}

void TofOutv2::set_tof(const unsigned int ihit, const float rval)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_tof(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}

float TofOutv2::get_tof_err(const unsigned int ihit) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_tof_err() : NAN);
}

void TofOutv2::set_tof_err(const unsigned int ihit, const float rval)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_tof_err(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}


float TofOutv2::get_xtof(const unsigned int ihit, const short int ival) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_xtof(ival) : NAN);
}

void TofOutv2::set_xtof(const unsigned int ihit, const short i, const float rval)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_xtof(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}

float TofOutv2::get_xtof_err(const unsigned int ihit, const short int ival) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_xtof_err(ival) : NAN);
  }

void TofOutv2::set_xtof_err(const unsigned int ihit, const short i, const float rval)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_xtof_err(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}

float TofOutv2::get_tdiff(const unsigned int ihit) const
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  return ((tofhit) ? tofhit->get_tdiff() : NAN);
}

void TofOutv2::set_tdiff(const unsigned int ihit, const float rval)
{
  TofHit *tofhit = (TofHit *) GetTofHits()->UncheckedAt(ihit);
  if (tofhit)
    {
      tofhit->set_tdiff(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no TofHit object found" << endl;
    }
  return ;
}
