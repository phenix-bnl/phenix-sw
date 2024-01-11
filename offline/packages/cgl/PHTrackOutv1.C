#include "PHTrackOutv1.h"
#include "PHSnglTrackv1.h"

#include "phool.h"

#include "TClonesArray.h"


ClassImp(PHTrackOutv1)

static unsigned int PHNTRACKSV1 = 200;

using namespace std;

PHTrackOutv1::PHTrackOutv1()
{
  PHNTrack = 0;
  PHTrk = new TClonesArray("PHSnglTrackv1",PHNTRACKSV1);
  return;
}

PHTrackOutv1::~PHTrackOutv1()
{
  PHTrk->Clear();
  return;
}

void PHTrackOutv1::identify(ostream& os) const
{
  os << "identify yourself: PHTrackOutv1 Object" << endl;
  os << "No of Tracks: " << PHNTrack << endl;
  return;
}

void PHTrackOutv1::Reset()
{
  PHTrk->Clear();
  if (PHNTrack>PHNTRACKSV1)
    {
      PHTrk->Expand(PHNTRACKSV1);
    }
  PHNTrack = 0;
  return;
}

int PHTrackOutv1::isValid() const
{
  return((PHNTrack>0) ? 1 : 0);
}
unsigned int PHTrackOutv1::get_PHNTrack() const
{
  return PHNTrack;
}
void  PHTrackOutv1::set_PHNTrack(const unsigned int ntrk)
{
  PHNTrack = ntrk;
}

int PHTrackOutv1::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKSV1)
    {
      PHTrk->Expand(nhits);
    }
  return nhits;
}

void  PHTrackOutv1::AddPHTrack(const unsigned int itrk)
{
  TClonesArray &phtrk = *PHTrk;
  new(phtrk[itrk]) PHSnglTrackv1();
  return;
}

unsigned short PHTrackOutv1::get_trackIndex(const unsigned int itrk)   const 
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_trackIndex() : -1);
}

void  PHTrackOutv1::set_trackIndex(const unsigned int itrk, const unsigned short rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if(phtrk)
    {
      phtrk->set_trackIndex(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }

  return;
}

float PHTrackOutv1::get_projectionVtx(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionVtx(i) : -99999.9);
}

void PHTrackOutv1::set_projectionVtx(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionVtx(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_projectionDch(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionDch(i) : -99999.9);
}

void PHTrackOutv1::set_projectionDch(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionDch(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_projectionTec(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionTec(i) : -99999.9);
}

void PHTrackOutv1::set_projectionTec(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionTec(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_projectionPc1(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPc1(i) : -99999.9);
}

void PHTrackOutv1::set_projectionPc1(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPc1(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_projectionPc2(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPc2(i) : -99999.9);
}

void PHTrackOutv1::set_projectionPc2(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPc2(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_projectionPc3(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPc3(i) : -99999.9);
}

void PHTrackOutv1::set_projectionPc3(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPc3(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_projectionTof(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionTof(i) : -99999.9);
}

void PHTrackOutv1::set_projectionTof(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionTof(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_projectionPbGl(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPbGl(i) : -99999.9);
}

void PHTrackOutv1::set_projectionPbGl(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPbGl(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}
float PHTrackOutv1::get_projectionPbSc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPbSc(i) : -99999.9);
}

void PHTrackOutv1::set_projectionPbSc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPbSc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_projectionCrk(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionCrk(i) : -99999.9);
}

void PHTrackOutv1::set_projectionCrk(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionCrk(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_projectionEmc(const unsigned int itrk, const short i) const
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (get_projectionPbSc(itrk,1) > -99999.)
    {
      return((phtrk) ? phtrk->get_projectionPbSc(i) : -99999.9);
    } 
  if (get_projectionPbGl(itrk,1) > -99999.)
    {
      return((phtrk) ? phtrk->get_projectionPbGl(i) : -99999.9);
    } 
  return -99999.9;
}

float PHTrackOutv1::get_directionVtx(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionVtx(i) : -99999.9);
}

void PHTrackOutv1::set_directionVtx(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionVtx(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_directionDch(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionDch(i) : -99999.9);
}

void PHTrackOutv1::set_directionDch(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionDch(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_directionTec(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionTec(i) : -99999.9);
}

void PHTrackOutv1::set_directionTec(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionTec(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_directionPc1(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPc1(i) : -99999.9);
}

void PHTrackOutv1::set_directionPc1(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPc1(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_directionPc2(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPc2(i) : -99999.9);
}

void PHTrackOutv1::set_directionPc2(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPc2(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_directionPc3(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPc3(i) : -99999.9);
}

void PHTrackOutv1::set_directionPc3(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPc3(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_directionTof(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionTof(i) : -99999.9);
}

void PHTrackOutv1::set_directionTof(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionTof(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_directionPbGl(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPbGl(i) : -99999.9);
}

void PHTrackOutv1::set_directionPbGl(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPbGl(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}
float PHTrackOutv1::get_directionPbSc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPbSc(i) : -99999.9);
}

void PHTrackOutv1::set_directionPbSc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPbSc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_directionCrk(const unsigned int itrk, const short i) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionCrk(i) : -99999.9);
}

void PHTrackOutv1::set_directionCrk(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionCrk(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}


float PHTrackOutv1::get_emcPathLength(const unsigned int itrk) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_emcPathLength() : -99999.9);
}

void PHTrackOutv1::set_emcPathLength(const unsigned int itrk, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_emcPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

float PHTrackOutv1::get_tofPathLength(const unsigned int itrk) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_tofPathLength() : -99999.9);
}

void PHTrackOutv1::set_tofPathLength(const unsigned int itrk, const float rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_tofPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}


float PHTrackOutv1::get_crkPathLength(const unsigned int itrk) const
{

  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_crkPathLength() : -99999.9);
}

void PHTrackOutv1::set_crkPathLength(const unsigned int itrk, const float  rval)
{
  PHSnglTrackv1 *phtrk = (PHSnglTrackv1 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_crkPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv1 object found" << endl;
    }
  return;
}

short PHTrackOutv1::ifIntersectVtx(const unsigned int itrk) const
{
  return((get_projectionVtx(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv1::ifIntersectDch(const unsigned int itrk) const
{
  return((get_projectionDch(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv1::ifIntersectPc1(const unsigned int itrk) const
{
  return((get_projectionPc1(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv1::ifIntersectPc2(const unsigned int itrk) const
{
  return((get_projectionPc2(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv1::ifIntersectPc3(const unsigned int itrk) const
{
  return((get_projectionPc3(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv1::ifIntersectCrk(const unsigned int itrk) const
{
  return((get_projectionCrk(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv1::ifIntersectTec(const unsigned int itrk) const
{
  return((get_projectionTec(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv1::ifIntersectTof(const unsigned int itrk) const
{
  return((get_projectionTof(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv1::ifIntersectPbgl(const unsigned int itrk) const
{
  return((get_projectionPbGl(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv1::ifIntersectPbsc(const unsigned int itrk) const
{
  return((get_projectionPbSc(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv1::ifIntersectEmc(const unsigned int itrk) const
{
  if (get_projectionPbSc(itrk,1) > -99999.)
    {
      return 1;
    } 
  if (get_projectionPbGl(itrk,1) > -99999.)
    {
      return 1;
    } 
  return 0;
}
