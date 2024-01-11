#include <CrkRingMicrov4.h>
#include <CrkSnglRingMicrov3.h>
#include <phool.h>
#include <TClonesArray.h>
#include <iostream>

ClassImp(CrkRingMicrov4)

using namespace std;

static const unsigned int CRKNRING = 100;

CrkRingMicrov4::CrkRingMicrov4()
{
  CrkNRing = 0;
  CrkRings = new TClonesArray("CrkSnglRingMicrov3",CRKNRING);
  return;
}

CrkRingMicrov4::~CrkRingMicrov4()
{
  CrkRings->Clear();
  delete CrkRings;
  return;
}

void CrkRingMicrov4::identify(ostream& os) const
{
  os << "identify yourself: CrkRingMicrov4 Object" << std::endl;
  os << "No of Tracks: " << CrkNRing << std::endl;
  return;
}

void CrkRingMicrov4::Reset()
{
 CrkRings->Clear();
 if (CrkNRing>CRKNRING)
   {
     CrkRings->Expand(CRKNRING);
   }
 CrkNRing = 0;
 return;
}

int CrkRingMicrov4::isValid() const
{
  return((CrkNRing>0) ? 1 : 0);
}

int CrkRingMicrov4::set_TClonesArraySize(const unsigned int nring)
{
  if (nring > CRKNRING)
    {
      CrkRings->Expand(nring);
     }
  return nring;
}

void  CrkRingMicrov4::AddCrkRing(const unsigned int iring)
{
  TClonesArray &crkring = *CrkRings;
  new(crkring[iring]) CrkSnglRingMicrov3();
  return;
}

short CrkRingMicrov4::get_accepted(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_accepted() : -999);
}

void CrkRingMicrov4::set_accepted(const unsigned int iring, const short ival)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_accepted(ival);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

short CrkRingMicrov4::get_panel(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_panel() : -999);
}

void CrkRingMicrov4::set_panel(const unsigned int iring, const short ival)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_panel(ival);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

short CrkRingMicrov4::get_npmt0(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npmt0() : -999);
}

void CrkRingMicrov4::set_npmt0(const unsigned int iring, const short ival)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npmt0(ival);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

short CrkRingMicrov4::get_npmt1(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npmt1() : -999);
}

void CrkRingMicrov4::set_npmt1(const unsigned int iring, const short ival)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npmt1(ival);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

short CrkRingMicrov4::get_npmt2(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npmt2() : -999);
}

void CrkRingMicrov4::set_npmt2(const unsigned int iring, const short ival)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npmt2(ival);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

short CrkRingMicrov4::get_npmt3(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npmt3() : -999);
}

void CrkRingMicrov4::set_npmt3(const unsigned int iring, const short ival)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npmt3(ival);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov4::get_npe0(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npe0() : -999);
}

void CrkRingMicrov4::set_npe0(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npe0(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov4::get_npe1(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npe1() : -999);
}

void CrkRingMicrov4::set_npe1(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npe1(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov4::get_npe2(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npe2() : -999);
}

void CrkRingMicrov4::set_npe2(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npe2(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov4::get_npe3(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_npe3() : -999);
}

void CrkRingMicrov4::set_npe3(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_npe3(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov4::get_chi2(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_chi2() : -999);
}

void CrkRingMicrov4::set_chi2(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_chi2(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov4::get_disp(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_disp() : -999);
}

void CrkRingMicrov4::set_disp(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_disp(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov4::get_cross_phi(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_cross_phi() : -999);
}

void CrkRingMicrov4::set_cross_phi(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_cross_phi(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov4::get_cross_z(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_cross_z() : -999);
}

void CrkRingMicrov4::set_cross_z(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_cross_z(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov4::get_center_phi(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_center_phi() : -999);
}

void CrkRingMicrov4::set_center_phi(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_center_phi(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

float CrkRingMicrov4::get_center_z(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_center_z() : -999);
}

void CrkRingMicrov4::set_center_z(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_center_z(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}

// Added time to the CRK rings...TKH 5-20-2002.
float CrkRingMicrov4::get_tcrk(const unsigned int iring) const
{

  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_tcrk() : -999);
}

void CrkRingMicrov4::set_tcrk(const unsigned int iring, const float rval)
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_tcrk(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
  }
  return;
}


short CrkRingMicrov4::get_cgltrackid(const unsigned int iring) const //FM
{
  CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  return((crkring) ? crkring->get_cgltrackid() : -999);
}


void CrkRingMicrov4::set_cgltrackid(const unsigned int iring, const unsigned int itrk) //FM
{

 CrkSnglRingMicrov3 *crkring = (CrkSnglRingMicrov3 *) GetCrkRing()->UncheckedAt(iring);
  if (crkring)
    {
      crkring->set_cgltrackid(itrk);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no CrkSnglRingMicrov3 object found" << std::endl;
    }
  return;
}

