#include "CrkHitExtv1.h"
#include "CrkSnglHitExtv1.h"
#include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(CrkHitExtv1)

using namespace std;

static unsigned int CRKNHIT = 100;

CrkHitExtv1::CrkHitExtv1()
{
  CrkNHit = 0;
  Crk = new TClonesArray("CrkSnglHitExtv1", CRKNHIT);
  return ;
}

CrkHitExtv1::~CrkHitExtv1()
{
  if (Crk)
    {
      Crk->Clear();
      delete Crk;
    }
  return ;
}

void CrkHitExtv1::identify(ostream& os) const
{
  os << "identify yourself: CrkHitExtv1 Object" << endl;
  os << "No of Hits: " << CrkNHit << endl;
  return ;
}

void CrkHitExtv1::Reset()
{
  Crk->Clear();
  if (CrkNHit > CRKNHIT)
    {
      Crk->Expand(CRKNHIT);
    }
  CrkNHit = 0;
  return ;
}

int CrkHitExtv1::isValid() const
{
  return ((CrkNHit > 0) ? 1 : 0);
}

int CrkHitExtv1::set_TClonesArraySize(const unsigned int nHit)
{
  if (nHit > CRKNHIT)
    {
      Crk->Expand(nHit);
    }
  return nHit;
}

void CrkHitExtv1::AddCrkHitExt(const unsigned int iHit)
{
  TClonesArray &crkHit = *Crk;
  new(crkHit[iHit]) CrkSnglHitExtv1();
  return ;
}

short CrkHitExtv1::get_pmt(const unsigned int iHit) const
{
  CrkSnglHitExtv1 *crkHit = (CrkSnglHitExtv1 *) GetCrk()->UncheckedAt(iHit);
  return ((crkHit) ? crkHit->get_pmt() : -999);
}

void CrkHitExtv1::set_pmt(const unsigned int iHit, const short ival)
{
  CrkSnglHitExtv1 *crkHit = (CrkSnglHitExtv1 *) GetCrk()->UncheckedAt(iHit);
  if (crkHit)
    {
      crkHit->set_pmt(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no CrkSnglHitExtv1 object found" << endl;
    }
  return ;
}


float CrkHitExtv1::get_npe(const unsigned int iHit) const
{
  CrkSnglHitExtv1 *crkHit = (CrkSnglHitExtv1 *) GetCrk()->UncheckedAt(iHit);
  return ((crkHit) ? crkHit->get_npe() : -999);
}

void CrkHitExtv1::set_npe(const unsigned int iHit, const float rval)
{
  CrkSnglHitExtv1 *crkHit = (CrkSnglHitExtv1 *) GetCrk()->UncheckedAt(iHit);
  if (crkHit)
    {
      crkHit->set_npe(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no CrkSnglHitExtv1 object found" << endl;
    }
  return ;
}

float CrkHitExtv1::get_time(const unsigned int iHit) const
{
  CrkSnglHitExtv1 *crkHit = (CrkSnglHitExtv1 *) GetCrk()->UncheckedAt(iHit);
  return ((crkHit) ? crkHit->get_time() : -999);
}

void CrkHitExtv1::set_time(const unsigned int iHit, const float rval)
{
  CrkSnglHitExtv1 *crkHit = (CrkSnglHitExtv1 *) GetCrk()->UncheckedAt(iHit);
  if (crkHit)
    {
      crkHit->set_time(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no CrkSnglHitExtv1 object found" << endl;
    }
  return ;
}

float CrkHitExtv1::get_posX(const unsigned int iHit) const
{
  CrkSnglHitExtv1 *crkHit = (CrkSnglHitExtv1 *) GetCrk()->UncheckedAt(iHit);
  return ((crkHit) ? crkHit->get_posX() : -999);
}

void CrkHitExtv1::set_posX(const unsigned int iHit, const float rval)
{
  CrkSnglHitExtv1 *crkHit = (CrkSnglHitExtv1 *) GetCrk()->UncheckedAt(iHit);
  if (crkHit)
    {
      crkHit->set_posX(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no CrkSnglHitExtv1 object found" << endl;
    }
  return ;
}

float CrkHitExtv1::get_posY(const unsigned int iHit) const
{
  CrkSnglHitExtv1 *crkHit = (CrkSnglHitExtv1 *) GetCrk()->UncheckedAt(iHit);
  return ((crkHit) ? crkHit->get_posY() : -999);
}

void CrkHitExtv1::set_posY(const unsigned int iHit, const float rval)
{
  CrkSnglHitExtv1 *crkHit = (CrkSnglHitExtv1 *) GetCrk()->UncheckedAt(iHit);
  if (crkHit)
    {
      crkHit->set_posY(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no CrkSnglHitExtv1 object found" << endl;
    }
  return ;
}

float CrkHitExtv1::get_posZ(const unsigned int iHit) const
{
  CrkSnglHitExtv1 *crkHit = (CrkSnglHitExtv1 *) GetCrk()->UncheckedAt(iHit);
  return ((crkHit) ? crkHit->get_posZ() : -999);
}

void CrkHitExtv1::set_posZ(const unsigned int iHit, const float rval)
{
  CrkSnglHitExtv1 *crkHit = (CrkSnglHitExtv1 *) GetCrk()->UncheckedAt(iHit);
  if (crkHit)
    {
      crkHit->set_posZ(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no CrkSnglHitExtv1 object found" << endl;
    }
  return ;
}

