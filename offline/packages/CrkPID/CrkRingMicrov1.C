#include "CrkRingMicrov1.h"
#include "CrkSnglRingMicrov1.h"
#include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(CrkRingMicrov1)

using namespace std;

static const unsigned int CRKNRING = 100;

CrkRingMicrov1::CrkRingMicrov1()
{
  CrkNRing = 0;
  CrkRing = new TClonesArray("CrkSnglRingMicrov1",CRKNRING);
  return;
}

CrkRingMicrov1::~CrkRingMicrov1()
{
  CrkRing->Clear();
  if (CrkRing)
    {
      delete CrkRing;
    }
  return;
}

void CrkRingMicrov1::identify(ostream& os) const
{
  os << "identify yourself: CrkRingMicrov1 Object" << std::endl;
  os << "No of Tracks: " << CrkNRing << std::endl;
  return;
}

void CrkRingMicrov1::Reset()
{
 CrkRing->Clear();
 if (CrkNRing>CRKNRING)
   {
     CrkRing->Expand(CRKNRING);
   }
 CrkNRing = 0;
 return;
}

int CrkRingMicrov1::isValid() const
{
  return((CrkNRing>0) ? 1 : 0);
}

int CrkRingMicrov1::set_TClonesArraySize(const unsigned int nring)
{
  if (nring > CRKNRING)
    {
      CrkRing->Expand(nring);
     }
  return nring;
}

void  CrkRingMicrov1::AddCrkRing(const unsigned int iring)
{
  TClonesArray &crkring = *CrkRing;
  new(crkring[iring]) CrkSnglRingMicrov1();
  return;
}

short CrkRingMicrov1::get_accepted(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_accepted() : -999);
}

void CrkRingMicrov1::set_accepted(const unsigned int iring, const short ival)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_accepted(ival);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

short CrkRingMicrov1::get_panel(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_panel() : -999);
}

void CrkRingMicrov1::set_panel(const unsigned int iring, const short ival)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_panel(ival);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

short CrkRingMicrov1::get_npmt0(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npmt0() : -999);
}

void CrkRingMicrov1::set_npmt0(const unsigned int iring, const short ival)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npmt0(ival);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

short CrkRingMicrov1::get_npmt1(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npmt1() : -999);
}

void CrkRingMicrov1::set_npmt1(const unsigned int iring, const short ival)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npmt1(ival);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

short CrkRingMicrov1::get_npmt3(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npmt3() : -999);
}

void CrkRingMicrov1::set_npmt3(const unsigned int iring, const short ival)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npmt3(ival);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov1::get_npe0(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npe0() : -999);
}

void CrkRingMicrov1::set_npe0(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npe0(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov1::get_npe1(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npe1() : -999);
}

void CrkRingMicrov1::set_npe1(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npe1(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov1::get_npe3(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npe3() : -999);
}

void CrkRingMicrov1::set_npe3(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npe3(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov1::get_chi2(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_chi2() : -999);
}

void CrkRingMicrov1::set_chi2(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_chi2(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov1::get_disp(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_disp() : -999);
}

void CrkRingMicrov1::set_disp(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_disp(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov1::get_cross_phi(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_cross_phi() : -999);
}

void CrkRingMicrov1::set_cross_phi(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_cross_phi(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov1::get_cross_z(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_cross_z() : -999);
}

void CrkRingMicrov1::set_cross_z(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_cross_z(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov1::get_center_phi(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_center_phi() : -999);
}

void CrkRingMicrov1::set_center_phi(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_center_phi(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov1::get_center_z(const unsigned int iring) const
{

  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_center_z() : -999);
}

void CrkRingMicrov1::set_center_z(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov1 *crkring = (CrkSnglRingMicrov1 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_center_z(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov1 object found" << std::endl;
  }
  return;
}

