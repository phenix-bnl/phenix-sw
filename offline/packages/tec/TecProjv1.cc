#include <TecProjv1.hh>
#include <TecSnglProjv1.hh>
#include <phool.h>
#include <TClonesArray.h>
#include <iostream>

ClassImp(TecProjv1)

using namespace std;

static const unsigned int TECNTRK = 350;

TecProjv1::TecProjv1()
{
  TecNProj = 0;
  TecProj = new TClonesArray("TecSnglProjv1",TECNTRK);
  return;
}

TecProjv1::~TecProjv1()
{
  TecProj->Clear();
  if(TecProj)
    delete TecProj;
  return;
}

void TecProjv1::identify(ostream& os) const
{
  os << "identify yourself: TecProjv1 Object" << std::endl;
  os << "No of Tracks: " << TecNProj << std::endl;
  return;
}

void TecProjv1::Reset()
{
 TecProj->Clear();
 if (TecNProj>TECNTRK)
   {
     TecProj->Expand(TECNTRK);
   }
 TecNProj = 0;
 return;
}

int TecProjv1::isValid() const
{
  return((TecNProj>0) ? 1 : 0);
}

int TecProjv1::set_TClonesArraySize(const unsigned int ntrk)
{
  if (ntrk > TECNTRK)
    {
      TecProj->Expand(ntrk);
     }
  return ntrk;
}

void  TecProjv1::AddTecProj(const unsigned int iproj)
{
  TClonesArray &tecproj = *TecProj;
  new(tecproj[iproj]) TecSnglProjv1();
  return;
}

// Start point getters
//-----------------------
float TecProjv1::get_pstartx(const unsigned int iproj) const
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  return((tecproj) ? tecproj->get_pstartx() : -999);
}

float TecProjv1::get_pstarty(const unsigned int iproj) const
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  return((tecproj) ? tecproj->get_pstarty() : -999);
}

float TecProjv1::get_pstartz(const unsigned int iproj) const
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  return((tecproj) ? tecproj->get_pstartz() : -999);
}

// End point getters
//-----------------------

float TecProjv1::get_pendx(const unsigned int iproj) const
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  return((tecproj) ? tecproj->get_pendx() : -999);
}

float TecProjv1::get_pendy(const unsigned int iproj) const
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  return((tecproj) ? tecproj->get_pendy() : -999);
}

float TecProjv1::get_pendz(const unsigned int iproj) const
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  return((tecproj) ? tecproj->get_pendz() : -999);
}

// Start point setters
//-----------------------

void TecProjv1::set_pstartx(const unsigned int iproj, const float rval)
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  if (tecproj)
    {
      tecproj->set_pstartx(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no TecSnglProjv1 object found" << std::endl;
  }
  return;
}

void TecProjv1::set_pstarty(const unsigned int iproj, const float rval)
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  if (tecproj)
    {
      tecproj->set_pstarty(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no TecSnglProjv1 object found" << std::endl;
  }
  return;
}

void TecProjv1::set_pstartz(const unsigned int iproj, const float rval)
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  if (tecproj)
    {
      tecproj->set_pstartz(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no TecSnglProjv1 object found" << std::endl;
  }
  return;
}

// End point setters
//---------------------

void TecProjv1::set_pendx(const unsigned int iproj, const float rval)
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  if (tecproj)
    {
      tecproj->set_pendx(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no TecSnglProjv1 object found" << std::endl;
  }
  return;
}

void TecProjv1::set_pendy(const unsigned int iproj, const float rval)
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  if (tecproj)
    {
      tecproj->set_pendy(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no TecSnglProjv1 object found" << std::endl;
  }
  return;
}

void TecProjv1::set_pendz(const unsigned int iproj, const float rval)
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  if (tecproj)
    {
      tecproj->set_pendz(rval);
    }
else
  {
    std::cout << PHWHERE << "ERROR no TecSnglProjv1 object found" << std::endl;
  }
  return;
}

// TecCluster ID
//--------------

int TecProjv1::get_teclusterid(const unsigned int iproj, const unsigned iplane) const
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  return((tecproj) ? tecproj->get_tecclusterid(iplane) : -999);
}


void TecProjv1::set_tecclusterid(const unsigned iproj, const unsigned int iplane, const int tecclusterid)
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  if (tecproj)
    {
      tecproj->set_tecclusterid(iplane, tecclusterid);
    }
else
  {
    std::cout << PHWHERE << "ERROR no TecSnglProjv1 object found" << std::endl;
  }
  return;  
}

// track ID
//---------

short TecProjv1::get_cgltrackid(const unsigned int iproj) const 
{
  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  return((tecproj) ? tecproj->get_cgltrackid() : -999);
}

void TecProjv1::set_cgltrackid(const unsigned int iproj, const unsigned int itrk)
{

  TecSnglProjv1 *tecproj = (TecSnglProjv1 *) GetTecProj()->UncheckedAt(iproj);
  if (tecproj)
    {
      tecproj->set_cgltrackid(itrk);
    }
  else
    {
      std::cout << PHWHERE << "ERROR no TecSnglProjv1 object found" << std::endl;
    }
  return;
}

