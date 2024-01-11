#include <CrkProjv1.h>
#include <CrkSnglProjv1.h>
#include <phool.h>
#include <TClonesArray.h>
#include <iostream>

ClassImp(CrkProjv1)

using namespace std;

static const unsigned int CRKNRING = 100;

CrkProjv1::CrkProjv1()
{
  CrkNRing = 0;
  CrkProj = new TClonesArray("CrkSnglProjv1",CRKNRING);
  return;
}

CrkProjv1::~CrkProjv1()
{
  CrkProj->Clear();
  if(CrkProj)
    delete CrkProj;
  return;
}

void CrkProjv1::identify(ostream& os) const
{
  os << "identify yourself: CrkProjv1 Object" << std::endl;
  os << "No of Tracks: " << CrkNRing << std::endl;
  return;
}

void CrkProjv1::Reset()
{
 CrkProj->Clear();
 if (CrkNRing>CRKNRING)
   {
     CrkProj->Expand(CRKNRING);
   }
 CrkNRing = 0;
 return;
}

int CrkProjv1::isValid() const
{
  return((CrkNRing>0) ? 1 : 0);
}

int CrkProjv1::set_TClonesArraySize(const unsigned int nring)
{
  if (nring > CRKNRING)
    {
      CrkProj->Expand(nring);
     }
  return nring;
}

void  CrkProjv1::AddCrkProj(const unsigned int iring)
{
  TClonesArray &crkproj = *CrkProj;
  new(crkproj[iring]) CrkSnglProjv1();
  return;
}

// Start point getters
//-----------------------
float CrkProjv1::get_pstartx(const unsigned int iring) const
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  return((crkproj) ? crkproj->get_pstartx() : -999);
}

float CrkProjv1::get_pstarty(const unsigned int iring) const
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  return((crkproj) ? crkproj->get_pstarty() : -999);
}

float CrkProjv1::get_pstartz(const unsigned int iring) const
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  return((crkproj) ? crkproj->get_pstartz() : -999);
}

// End point getters
//-----------------------

float CrkProjv1::get_pendx(const unsigned int iring) const
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  return((crkproj) ? crkproj->get_pendx() : -999);
}

float CrkProjv1::get_pendy(const unsigned int iring) const
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  return((crkproj) ? crkproj->get_pendy() : -999);
}

float CrkProjv1::get_pendz(const unsigned int iring) const
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  return((crkproj) ? crkproj->get_pendz() : -999);
}

// Start point setters
//-----------------------

void CrkProjv1::set_pstartx(const unsigned int iring, const float rval)
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  if (crkproj)
    {
      crkproj->set_pstartx(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglProjv1 object found" << std::endl;
  }
  return;
}

void CrkProjv1::set_pstarty(const unsigned int iring, const float rval)
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  if (crkproj)
    {
      crkproj->set_pstarty(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglProjv1 object found" << std::endl;
  }
  return;
}

void CrkProjv1::set_pstartz(const unsigned int iring, const float rval)
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  if (crkproj)
    {
      crkproj->set_pstartz(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglProjv1 object found" << std::endl;
  }
  return;
}

// End point setters
//---------------------

void CrkProjv1::set_pendx(const unsigned int iring, const float rval)
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  if (crkproj)
    {
      crkproj->set_pendx(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglProjv1 object found" << std::endl;
  }
  return;
}

void CrkProjv1::set_pendy(const unsigned int iring, const float rval)
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  if (crkproj)
    {
      crkproj->set_pendy(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglProjv1 object found" << std::endl;
  }
  return;
}

void CrkProjv1::set_pendz(const unsigned int iring, const float rval)
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  if (crkproj)
    {
      crkproj->set_pendz(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no CrkSnglProjv1 object found" << std::endl;
  }
  return;
}

// ring and track ID
//------------------

short CrkProjv1::get_ringid(const unsigned int iring) const 
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  return((crkproj) ? crkproj->get_ringid() : -999);
}

void CrkProjv1::set_ringid(const unsigned int iring, const unsigned int inring) //FM
{

 CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  if (crkproj)
    {
      crkproj->set_ringid(inring);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no CrkSnglProjv1 object found" << std::endl;
    }
  return;
}

short CrkProjv1::get_cgltrackid(const unsigned int iring) const 
{
  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  return((crkproj) ? crkproj->get_cgltrackid() : -999);
}

void CrkProjv1::set_cgltrackid(const unsigned int iring, const unsigned int itrk) //FM
{

  CrkSnglProjv1 *crkproj = (CrkSnglProjv1 *) GetCrkProj()->UncheckedAt(iring);
  if (crkproj)
    {
      crkproj->set_cgltrackid(itrk);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no CrkSnglProjv1 object found" << std::endl;
    }
  return;
}

