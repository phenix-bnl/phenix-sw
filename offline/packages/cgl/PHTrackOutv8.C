#include "PHTrackOutv8.h"
#include "PHSnglTrackv8.h"

#include "phool.h"

#include "TClonesArray.h"


ClassImp(PHTrackOutv8)

static unsigned int PHNTRACKSV8 = 200;

using namespace std;

PHTrackOutv8::PHTrackOutv8()
{
  PHTrk = new TClonesArray("PHSnglTrackv8",PHNTRACKSV8);
  return;
}

PHTrackOutv8::~PHTrackOutv8()
{
  if (PHTrk)
    {
      PHTrk->Clear();
      delete PHTrk;
    }
  return;
}

void PHTrackOutv8::Reset()
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
  if (get_PHNTrack() > PHNTRACKSV8)
    {
      PHTrk->Expand(PHNTRACKSV8);
    }
  return ;
}

unsigned int
PHTrackOutv8::get_PHNTrack() const
{
  return GetPHTrk()->GetEntriesFast();
}

int PHTrackOutv8::isValid() const
{
  return ((get_PHNTrack() > 0) ? 1 : 0);
}

void PHTrackOutv8::identify(ostream& os) const
{
  os << "identify yourself: PHTrackOutv8 Object" << endl;
  os << "No of Tracks: " << get_PHNTrack() << endl;
  return;
}

void  PHTrackOutv8::AddPHTrack(const unsigned int itrk)
{
  TClonesArray &phtrk = *PHTrk;
  new(phtrk[itrk]) PHSnglTrackv8();
  return;
}

unsigned short PHTrackOutv8::get_trackIndex(const unsigned int itrk)   const 
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_trackIndex() : -1);
}

void  PHTrackOutv8::set_trackIndex(const unsigned int itrk, const unsigned short rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if(phtrk)
    {
      phtrk->set_trackIndex(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }

  return;
}

float PHTrackOutv8::get_projectionVtx(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionVtx(i) : -99999.9);
}

void PHTrackOutv8::set_projectionVtx(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionVtx(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_projectionDch(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionDch(i) : -99999.9);
}

void PHTrackOutv8::set_projectionDch(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionDch(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_projectionTec(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionTec(i) : -99999.9);
}

void PHTrackOutv8::set_projectionTec(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionTec(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_projectionPc1(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPc1(i) : -99999.9);
}

void PHTrackOutv8::set_projectionPc1(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPc1(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_projectionPc2(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPc2(i) : -99999.9);
}

void PHTrackOutv8::set_projectionPc2(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPc2(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_projectionPc3(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPc3(i) : -99999.9);
}

void PHTrackOutv8::set_projectionPc3(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPc3(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_projectionTof(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionTof(i) : -99999.9);
}

void PHTrackOutv8::set_projectionTof(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionTof(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_projectionTofw(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return ((phtrk) ? phtrk->get_projectionTofw(i) : -99999.9);
}

void PHTrackOutv8::set_projectionTofw(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionTofw(i, rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return ;
}

float PHTrackOutv8::get_projectionPbGl(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPbGl(i) : -99999.9);
}

void PHTrackOutv8::set_projectionPbGl(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPbGl(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}
float PHTrackOutv8::get_projectionPbSc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPbSc(i) : -99999.9);
}

void PHTrackOutv8::set_projectionPbSc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPbSc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_projectionCrk(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionCrk(i) : -99999.9);
}

void PHTrackOutv8::set_projectionCrk(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionCrk(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}
float PHTrackOutv8::get_projectionAcc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionAcc(i) : -99999.9);
}

void PHTrackOutv8::set_projectionAcc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionAcc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_projectionHbd(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionHbd(i) : -99999.9);
}

void PHTrackOutv8::set_projectionHbd(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionHbd(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_projectionEmc(const unsigned int itrk, const short i) const
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
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

float PHTrackOutv8::get_directionVtx(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionVtx(i) : -99999.9);
}

void PHTrackOutv8::set_directionVtx(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionVtx(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_directionDch(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionDch(i) : -99999.9);
}

void PHTrackOutv8::set_directionDch(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionDch(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_directionTec(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionTec(i) : -99999.9);
}

void PHTrackOutv8::set_directionTec(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionTec(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_directionPc1(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPc1(i) : -99999.9);
}

void PHTrackOutv8::set_directionPc1(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPc1(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_directionPc2(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPc2(i) : -99999.9);
}

void PHTrackOutv8::set_directionPc2(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPc2(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_directionPc3(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPc3(i) : -99999.9);
}

void PHTrackOutv8::set_directionPc3(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPc3(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_directionTof(const unsigned int itrk, const short i) const
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionTof(i) : -99999.9);
}

void PHTrackOutv8::set_directionTof(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionTof(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_directionTofw(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return ((phtrk) ? phtrk->get_directionTofw(i) : -99999.9);
}

void PHTrackOutv8::set_directionTofw(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionTofw(i, rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return ;
}

float PHTrackOutv8::get_directionPbGl(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPbGl(i) : -99999.9);
}

void PHTrackOutv8::set_directionPbGl(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPbGl(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}
float PHTrackOutv8::get_directionPbSc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPbSc(i) : -99999.9);
}

void PHTrackOutv8::set_directionPbSc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPbSc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}
float PHTrackOutv8::get_directionAcc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionAcc(i) : -99999.9);
}

void PHTrackOutv8::set_directionAcc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionAcc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_directionHbd(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionHbd(i) : -99999.9);
}

void PHTrackOutv8::set_directionHbd(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionHbd(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_directionCrk(const unsigned int itrk, const short i) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionCrk(i) : -99999.9);
}

void PHTrackOutv8::set_directionCrk(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionCrk(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}


float PHTrackOutv8::get_emcPathLength(const unsigned int itrk) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_emcPathLength() : -99999.9);
}

void PHTrackOutv8::set_emcPathLength(const unsigned int itrk, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_emcPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_tofPathLength(const unsigned int itrk) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_tofPathLength() : -99999.9);
}

void PHTrackOutv8::set_tofPathLength(const unsigned int itrk, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_tofPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_tofwPathLength(const unsigned int itrk) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_tofwPathLength() : -99999.9);
}

void PHTrackOutv8::set_tofwPathLength(const unsigned int itrk, const float rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_tofwPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}

float PHTrackOutv8::get_crkPathLength(const unsigned int itrk) const
{

  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_crkPathLength() : -99999.9);
}

void PHTrackOutv8::set_crkPathLength(const unsigned int itrk, const float  rval)
{
  PHSnglTrackv8 *phtrk = (PHSnglTrackv8 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_crkPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv8 object found" << endl;
    }
  return;
}


short PHTrackOutv8::ifIntersectVtx(const unsigned int itrk) const
{
  return ((get_projectionVtx(itrk, 1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv8::ifIntersectDch(const unsigned int itrk) const
{
  return ((get_projectionDch(itrk, 1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv8::ifIntersectPc1(const unsigned int itrk) const
{
  return ((get_projectionPc1(itrk, 1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv8::ifIntersectPc2(const unsigned int itrk) const
{
  return ((get_projectionPc2(itrk, 1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv8::ifIntersectPc3(const unsigned int itrk) const
{
  return ((get_projectionPc3(itrk, 1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv8::ifIntersectCrk(const unsigned int itrk) const
{
  return ((get_projectionCrk(itrk, 1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv8::ifIntersectTec(const unsigned int itrk) const
{
  return ((get_projectionTec(itrk, 1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv8::ifIntersectTof(const unsigned int itrk) const
{
  return ((get_projectionTof(itrk, 1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv8::ifIntersectTofw(const unsigned int itrk) const
{
  return ((get_projectionTofw(itrk, 1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv8::ifIntersectPbgl(const unsigned int itrk) const
{
  return ((get_projectionPbGl(itrk, 1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv8::ifIntersectPbsc(const unsigned int itrk) const
{
  return ((get_projectionPbSc(itrk, 1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv8::ifIntersectAcc(const unsigned int itrk) const
{
  return ((get_projectionAcc(itrk, 1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv8::ifIntersectEmc(const unsigned int itrk) const
{
  if (get_projectionPbSc(itrk, 1) > -99999.)
      {
        return 1;
      }
  if (get_projectionPbGl(itrk, 1) > -99999.)
    {
        return 1;
      }
  return 0;
}

short PHTrackOutv8::ifIntersectHbd(const unsigned int itrk) const
{
  return((get_projectionHbd(itrk,1) > -99999. ) ? 1 : 0);
}

