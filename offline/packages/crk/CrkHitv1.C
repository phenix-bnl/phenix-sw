#include "dCrkHitWrapper.h"
#include "CrkHitv1.h"
#include "CrkSnglHitv1.h"
#include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(CrkHitv1)

using namespace std;

static unsigned int CRKNHIT = 100;

CrkHitv1::CrkHitv1()
{
  CrkNHit = 0;
  Crk = new TClonesArray("CrkSnglHitv1", CRKNHIT);
  return ;
}

CrkHitv1::~CrkHitv1()
{
  if (Crk)
    {
      Crk->Clear();
      delete Crk;
    }
  return ;
}

void CrkHitv1::identify(ostream& os) const
{
  os << "identify yourself: CrkHitv1 Object" << endl;
  os << "No of Hits: " << CrkNHit << endl;
  return ;
}

void CrkHitv1::Reset()
{
  Crk->Clear();
  if (CrkNHit > CRKNHIT)
    {
      Crk->Expand(CRKNHIT);
    }
  CrkNHit = 0;
  return ;
}

int CrkHitv1::isValid() const
{
  return ((CrkNHit > 0) ? 1 : 0);
}

int CrkHitv1::set_TClonesArraySize(const unsigned int nHit)
{
  if (nHit > CRKNHIT)
    {
      Crk->Expand(nHit);
    }
  return nHit;
}

void CrkHitv1::AddCrkHit(const unsigned int iHit)
{
  TClonesArray &crkHit = *Crk;
  new(crkHit[iHit]) CrkSnglHitv1();
  return ;
}

short CrkHitv1::get_pmt(const unsigned int iHit) const
{

  CrkSnglHitv1 *crkHit = (CrkSnglHitv1 *) GetCrk()->UncheckedAt(iHit);
  return ((crkHit) ? crkHit->get_pmt() : -999);
}

void CrkHitv1::set_pmt(const unsigned int iHit, const short ival)
{
  CrkSnglHitv1 *crkHit = (CrkSnglHitv1 *) GetCrk()->UncheckedAt(iHit);
  if (crkHit)
    {
      crkHit->set_pmt(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no CrkSnglHitv1 object found" << endl;
    }
  return ;
}


float CrkHitv1::get_npe(const unsigned int iHit) const
{

  CrkSnglHitv1 *crkHit = (CrkSnglHitv1 *) GetCrk()->UncheckedAt(iHit);
  return ((crkHit) ? crkHit->get_npe() : -999);
}

void CrkHitv1::set_npe(const unsigned int iHit, const float rval)
{
  CrkSnglHitv1 *crkHit = (CrkSnglHitv1 *) GetCrk()->UncheckedAt(iHit);
  if (crkHit)
    {
      crkHit->set_npe(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no CrkSnglHitv1 object found" << endl;
    }
  return ;
}

float CrkHitv1::get_time(const unsigned int iHit) const
{

  CrkSnglHitv1 *crkHit = (CrkSnglHitv1 *) GetCrk()->UncheckedAt(iHit);
  return ((crkHit) ? crkHit->get_time() : -999);
}

void CrkHitv1::set_time(const unsigned int iHit, const float rval)
{
  CrkSnglHitv1 *crkHit = (CrkSnglHitv1 *) GetCrk()->UncheckedAt(iHit);
  if (crkHit)
    {
      crkHit->set_time(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no CrkSnglHitv1 object found" << endl;
    }
  return ;
}

void CrkHitv1::FillFromWrapper(dCrkHitWrapper *wrap)
{
  unsigned int ihit;
  if (wrap)
    {
      CrkNHit = wrap->RowCount();
      set_TClonesArraySize(wrap->RowCount());
      for (ihit = 0; ihit < wrap->RowCount();ihit++)
        {
          AddCrkHit(ihit);
          set_pmt(ihit, wrap->get_pmt(ihit));
          set_npe(ihit, wrap->get_npe(ihit));
          set_time(ihit, wrap->get_time(ihit));
        }
    }
  return ;
}
