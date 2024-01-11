#include "PHTrackOutv7.h"
#include "PHSnglTrackv7.h"

#include "phool.h"

#include "TClonesArray.h"


ClassImp(PHTrackOutv7)

static unsigned int PHNTRACKSV7 = 200;

using namespace std;

PHTrackOutv7::PHTrackOutv7()
{
  PHNTrack = 0;
  PHTrk = new TClonesArray("PHSnglTrackv7",PHNTRACKSV7);
  return;
}

PHTrackOutv7::~PHTrackOutv7()
{
  if (PHTrk)
    {
      PHTrk->Clear();
      delete PHTrk;
    }
  return;
}

void PHTrackOutv7::identify(ostream& os) const
{
  os << "identify yourself: PHTrackOutv7 Object" << endl;
  os << "No of Tracks: " << PHNTrack << endl;
  return;
}

void PHTrackOutv7::Reset()
{
  // the singles class need to be resetted as well,
  // In this implementation root makes the classes only once
  // in the TC->ExpandCreate() and then leaves them alive but
  // does not reset the values
  for (unsigned int i = 0; i < get_PHNTrack(); i++)
    {
      get_track(i)->Reset();
    }
  PHTrk->Clear();
  if (PHNTrack > PHNTRACKSV7)
    {
      PHTrk->Expand(PHNTRACKSV7);
    }
  PHNTrack = 0;
  return ;
}
int PHTrackOutv7::isValid() const
{
  return ((PHNTrack > 0) ? 1 : 0);
}

unsigned int PHTrackOutv7::get_PHNTrack() const
{
  return PHNTrack;
}
void PHTrackOutv7::set_PHNTrack(const unsigned int ntrk)
{
  PHNTrack = ntrk;
}

void  PHTrackOutv7::AddPHTrack(const unsigned int itrk)
{
  TClonesArray &phtrk = *PHTrk;
  new(phtrk[itrk]) PHSnglTrackv7();
  return;
}

unsigned short PHTrackOutv7::get_trackIndex(const unsigned int itrk)   const 
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_trackIndex() : -1);
}

void  PHTrackOutv7::set_trackIndex(const unsigned int itrk, const unsigned short rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if(phtrk)
    {
      phtrk->set_trackIndex(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }

  return;
}

float PHTrackOutv7::get_projectionVtx(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionVtx(i) : -99999.9);
}

void PHTrackOutv7::set_projectionVtx(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionVtx(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_projectionDch(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionDch(i) : -99999.9);
}

void PHTrackOutv7::set_projectionDch(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionDch(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_projectionTec(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionTec(i) : -99999.9);
}

void PHTrackOutv7::set_projectionTec(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionTec(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_projectionPc1(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPc1(i) : -99999.9);
}

void PHTrackOutv7::set_projectionPc1(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPc1(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_projectionPc2(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPc2(i) : -99999.9);
}

void PHTrackOutv7::set_projectionPc2(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPc2(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_projectionPc3(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPc3(i) : -99999.9);
}

void PHTrackOutv7::set_projectionPc3(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPc3(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_projectionTof(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionTof(i) : -99999.9);
}

void PHTrackOutv7::set_projectionTof(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionTof(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_projectionPbGl(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPbGl(i) : -99999.9);
}

void PHTrackOutv7::set_projectionPbGl(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPbGl(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}
float PHTrackOutv7::get_projectionPbSc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPbSc(i) : -99999.9);
}

void PHTrackOutv7::set_projectionPbSc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPbSc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_projectionCrk(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionCrk(i) : -99999.9);
}

void PHTrackOutv7::set_projectionCrk(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionCrk(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}
float PHTrackOutv7::get_projectionAcc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionAcc(i) : -99999.9);
}

void PHTrackOutv7::set_projectionAcc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionAcc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_projectionHbd(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionHbd(i) : -99999.9);
}

void PHTrackOutv7::set_projectionHbd(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionHbd(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_projectionEmc(const unsigned int itrk, const short i) const
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
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

float PHTrackOutv7::get_directionVtx(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionVtx(i) : -99999.9);
}

void PHTrackOutv7::set_directionVtx(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionVtx(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_directionDch(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionDch(i) : -99999.9);
}

void PHTrackOutv7::set_directionDch(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionDch(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_directionTec(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionTec(i) : -99999.9);
}

void PHTrackOutv7::set_directionTec(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionTec(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_directionPc1(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPc1(i) : -99999.9);
}

void PHTrackOutv7::set_directionPc1(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPc1(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_directionPc2(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPc2(i) : -99999.9);
}

void PHTrackOutv7::set_directionPc2(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPc2(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_directionPc3(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPc3(i) : -99999.9);
}

void PHTrackOutv7::set_directionPc3(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPc3(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_directionTof(const unsigned int itrk, const short i) const
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionTof(i) : -99999.9);
}

void PHTrackOutv7::set_directionTof(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionTof(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_directionPbGl(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPbGl(i) : -99999.9);
}

void PHTrackOutv7::set_directionPbGl(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPbGl(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}
float PHTrackOutv7::get_directionPbSc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPbSc(i) : -99999.9);
}

void PHTrackOutv7::set_directionPbSc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPbSc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}
float PHTrackOutv7::get_directionAcc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionAcc(i) : -99999.9);
}

void PHTrackOutv7::set_directionAcc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionAcc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_directionHbd(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionHbd(i) : -99999.9);
}

void PHTrackOutv7::set_directionHbd(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionHbd(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_directionCrk(const unsigned int itrk, const short i) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionCrk(i) : -99999.9);
}

void PHTrackOutv7::set_directionCrk(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionCrk(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}


float PHTrackOutv7::get_emcPathLength(const unsigned int itrk) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_emcPathLength() : -99999.9);
}

void PHTrackOutv7::set_emcPathLength(const unsigned int itrk, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_emcPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_tofPathLength(const unsigned int itrk) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_tofPathLength() : -99999.9);
}

void PHTrackOutv7::set_tofPathLength(const unsigned int itrk, const float rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_tofPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

float PHTrackOutv7::get_crkPathLength(const unsigned int itrk) const
{

  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_crkPathLength() : -99999.9);
}

void PHTrackOutv7::set_crkPathLength(const unsigned int itrk, const float  rval)
{
  PHSnglTrackv7 *phtrk = (PHSnglTrackv7 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_crkPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv7 object found" << endl;
    }
  return;
}

// Only add ifIntersectHbd here, and inherit other ifIntersect functions from v4.
short PHTrackOutv7::ifIntersectHbd(const unsigned int itrk) const
{
  return((get_projectionHbd(itrk,1) > -99999. ) ? 1 : 0);
}

