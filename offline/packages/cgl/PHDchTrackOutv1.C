#include "PHDchTrackOutv1.h"
#include "PHDchSnglTrackv1.h"
#include "dPHDchTrackWrapper.h"

//INCLUDECHECKER: Removed this line: #include "phool.h"

#include "TClonesArray.h"

ClassImp(PHDchTrackOutv1)

using namespace std;

#define PHDchNTRACKSV1 200  // default size of TClonesArray

PHDchTrackOutv1::PHDchTrackOutv1()
{
  PHDchNTrack = 0;
  PHDchTrk = new TClonesArray("PHDchSnglTrackv1",PHDchNTRACKSV1);
  return;
}

PHDchTrackOutv1::~PHDchTrackOutv1()
{
  if (PHDchTrk)
    {
      PHDchTrk->Clear();
      delete PHDchTrk;
    }
  return ;
}

void PHDchTrackOutv1::identify(ostream& os) const
{
  os << "identify yourself: PHDchTrackOutv1 Object" << endl;
  os << "No of Tracks: " << PHDchNTrack << endl;
  return;
}

void PHDchTrackOutv1::Reset()
{
 PHDchTrk->Clear();
 if (PHDchNTrack>PHDchNTRACKSV1)
   {
     PHDchTrk->Expand(PHDchNTRACKSV1);
   }
 PHDchNTrack = 0;
 return;
}

int PHDchTrackOutv1::isValid() const
{
  return((PHDchNTrack>0) ? 1 : 0);
}

int PHDchTrackOutv1::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHDchNTRACKSV1)
    {
      PHDchTrk->Expand(nhits);
     }
  return nhits;
}

void  PHDchTrackOutv1::AddPHDchTrack(const unsigned int itrk)
{
  TClonesArray &PHDchtrk = *PHDchTrk;
  new(PHDchtrk[itrk]) PHDchSnglTrackv1();
  return;
}

void PHDchTrackOutv1::FillFromWrapper(dPHDchTrackWrapper *wrap)
{
  unsigned int itrk;
  if (wrap)
    {
      PHDchNTrack = wrap->RowCount();
      set_TClonesArraySize(wrap->RowCount());
      for (itrk=0;itrk<wrap->RowCount();itrk++)
	{
	  AddPHDchTrack(itrk);
	  set_ErrorCode(itrk,wrap->get_ErrorCode(itrk));
	  set_numberOfSuccessfulIterations(itrk,wrap->get_numberOfSuccessfulIterations(itrk));
	  set_numberOfX1X2hitsFitted(itrk,wrap->get_numberOfX1X2hitsFitted(itrk));

	  set_chi2(itrk,wrap->get_chi2(itrk));
	  set_fittedAlpha(itrk,wrap->get_fittedAlpha(itrk));
	  set_fittedBeta(itrk,wrap->get_fittedBeta(itrk));
	  set_fittedPhi(itrk,wrap->get_fittedPhi(itrk));
	  set_fittedPhi0(itrk,wrap->get_fittedPhi0(itrk));
	  set_fittedTheta0(itrk,wrap->get_fittedTheta0(itrk));
	  set_momentum(itrk,wrap->get_momentum(itrk));
	}
    }
  return;
}

short PHDchTrackOutv1::get_ErrorCode(const unsigned int itrk) const
{

  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  return((phdchtrk) ? phdchtrk->get_ErrorCode() : -9999);
}

void PHDchTrackOutv1::set_ErrorCode(const unsigned int itrk, const short ival)
{
  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  if (phdchtrk)
    {
      phdchtrk->set_ErrorCode(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no PHDchSnglTrackv1 object found" << endl;
  }
  return;
}

short PHDchTrackOutv1::get_numberOfSuccessfulIterations(const unsigned int itrk) const
{

  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  return((phdchtrk) ? phdchtrk->get_numberOfSuccessfulIterations() : -9999);
}

void PHDchTrackOutv1::set_numberOfSuccessfulIterations(const unsigned int itrk, const short ival)
{
  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  if (phdchtrk)
    {
      phdchtrk->set_numberOfSuccessfulIterations(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no PHDchSnglTrackv1 object found" << endl;
  }
  return;
}

short PHDchTrackOutv1::get_numberOfX1X2hitsFitted(const unsigned int itrk) const
{

  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  return((phdchtrk) ? phdchtrk->get_numberOfX1X2hitsFitted() : -9999);
}

void PHDchTrackOutv1::set_numberOfX1X2hitsFitted(const unsigned int itrk, const short ival)
{
  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  if (phdchtrk)
    {
      phdchtrk->set_numberOfX1X2hitsFitted(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no PHDchSnglTrackv1 object found" << endl;
  }
  return;
}


float PHDchTrackOutv1::get_chi2(const unsigned int itrk) const
{

  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  return((phdchtrk) ? phdchtrk->get_chi2() : -99999.9);
}

void PHDchTrackOutv1::set_chi2(const unsigned int itrk, const float rval)
{
  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  if (phdchtrk)
    {
      phdchtrk->set_chi2(rval);
    }
else
  {
    cerr << PHWHERE << "ERROR no PHDchSnglTrackv1 object found" << endl;
  }
  return;
}

float PHDchTrackOutv1::get_fittedAlpha(const unsigned int itrk) const
{

  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  return((phdchtrk) ? phdchtrk->get_fittedAlpha() : -99999.9);
}

void PHDchTrackOutv1::set_fittedAlpha(const unsigned int itrk, const float rval)
{
  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  if (phdchtrk)
    {
      phdchtrk->set_fittedAlpha(rval);
    }
else
  {
    cerr << PHWHERE << "ERROR no PHDchSnglTrackv1 object found" << endl;
  }
  return;
}

float PHDchTrackOutv1::get_fittedBeta(const unsigned int itrk) const
{

  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  return((phdchtrk) ? phdchtrk->get_fittedBeta() : -99999.9);
}

void PHDchTrackOutv1::set_fittedBeta(const unsigned int itrk, const float rval)
{
  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  if (phdchtrk)
    {
      phdchtrk->set_fittedBeta(rval);
    }
else
  {
    cerr << PHWHERE << "ERROR no PHDchSnglTrackv1 object found" << endl;
  }
  return;
}

float PHDchTrackOutv1::get_fittedPhi(const unsigned int itrk) const
{

  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  return((phdchtrk) ? phdchtrk->get_fittedPhi() : -99999.9);
}

void PHDchTrackOutv1::set_fittedPhi(const unsigned int itrk, const float rval)
{
  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  if (phdchtrk)
    {
      phdchtrk->set_fittedPhi(rval);
    }
else
  {
    cerr << PHWHERE << "ERROR no PHDchSnglTrackv1 object found" << endl;
  }
  return;
}

float PHDchTrackOutv1::get_fittedPhi0(const unsigned int itrk) const
{

  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  return((phdchtrk) ? phdchtrk->get_fittedPhi0() : -99999.9);
}

void PHDchTrackOutv1::set_fittedPhi0(const unsigned int itrk, const float rval)
{
  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  if (phdchtrk)
    {
      phdchtrk->set_fittedPhi0(rval);
    }
else
  {
    cerr << PHWHERE << "ERROR no PHDchSnglTrackv1 object found" << endl;
  }
  return;
}

float PHDchTrackOutv1::get_fittedTheta0(const unsigned int itrk) const
{

  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  return((phdchtrk) ? phdchtrk->get_fittedTheta0() : -99999.9);
}

void PHDchTrackOutv1::set_fittedTheta0(const unsigned int itrk, const float rval)
{
  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  if (phdchtrk)
    {
      phdchtrk->set_fittedTheta0(rval);
    }
else
  {
    cerr << PHWHERE << "ERROR no PHDchSnglTrackv1 object found" << endl;
  }
  return;
}

float PHDchTrackOutv1::get_momentum(const unsigned int itrk) const
{

  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  return((phdchtrk) ? phdchtrk->get_momentum() : -99999.9);
}

void PHDchTrackOutv1::set_momentum(const unsigned int itrk, const float rval)
{
  PHDchSnglTrackv1 *phdchtrk = (PHDchSnglTrackv1 *) GetPHDchTrk()->UncheckedAt(itrk);
  if (phdchtrk)
    {
      phdchtrk->set_momentum(rval);
    }
else
  {
    cerr << PHWHERE << "ERROR no PHDchSnglTrackv1 object found" << endl;
  }
  return;
}


