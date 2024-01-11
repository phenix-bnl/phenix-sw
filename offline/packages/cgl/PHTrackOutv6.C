#include "PHTrackOutv6.h"
#include "PHSnglTrackv6.h"

#include "phool.h"

#include "TClonesArray.h"


ClassImp(PHTrackOutv6)

static unsigned int PHNTRACKSV6 = 200;

using namespace std;

PHTrackOutv6::PHTrackOutv6()
{
  PHNTrack = 0;
  PHTrk = new TClonesArray("PHSnglTrackv6",PHNTRACKSV6);
  return;
}

PHTrackOutv6::~PHTrackOutv6()
{
  if (PHTrk)
    {
      PHTrk->Clear();
      delete PHTrk;
    }
  return;
}

void PHTrackOutv6::identify(ostream& os) const
{
  os << "identify yourself: PHTrackOutv6 Object" << endl;
  os << "No of Tracks: " << PHNTrack << endl;
  return;
}

void PHTrackOutv6::Reset()
{
  // the singles class need to be resetted as well,
  // In this implementation root makes the classes only once
  // in the TC->ExpandCreate() and then leaves them alive but
  // does not reset the values
  for (unsigned int i = 0; i<get_PHNTrack(); i++)
    {
      get_track(i)->Reset();
    }
  PHTrk->Clear();
  if (PHNTrack>PHNTRACKSV6)
    {
      PHTrk->Expand(PHNTRACKSV6);
    }
  PHNTrack = 0;
  return;
}

int PHTrackOutv6::isValid() const
{
  return((PHNTrack>0) ? 1 : 0);
}
unsigned int PHTrackOutv6::get_PHNTrack() const
{
  return PHNTrack;
}
void  PHTrackOutv6::set_PHNTrack(const unsigned int ntrk)
{
  PHNTrack = ntrk;
}

void  PHTrackOutv6::AddPHTrack(const unsigned int itrk)
{
  TClonesArray &phtrk = *PHTrk;
  new(phtrk[itrk]) PHSnglTrackv6();
  return;
}

unsigned short PHTrackOutv6::get_trackIndex(const unsigned int itrk)   const 
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_trackIndex() : -1);
}

void  PHTrackOutv6::set_trackIndex(const unsigned int itrk, const unsigned short rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if(phtrk)
    {
      phtrk->set_trackIndex(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }

  return;
}

float PHTrackOutv6::get_projectionVtx(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionVtx(i) : -99999.9);
}

void PHTrackOutv6::set_projectionVtx(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionVtx(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_projectionDch(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionDch(i) : -99999.9);
}

void PHTrackOutv6::set_projectionDch(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionDch(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_projectionTec(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionTec(i) : -99999.9);
}

void PHTrackOutv6::set_projectionTec(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionTec(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_projectionPc1(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPc1(i) : -99999.9);
}

void PHTrackOutv6::set_projectionPc1(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPc1(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_projectionPc2(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPc2(i) : -99999.9);
}

void PHTrackOutv6::set_projectionPc2(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPc2(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_projectionPc3(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPc3(i) : -99999.9);
}

void PHTrackOutv6::set_projectionPc3(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPc3(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_projectionSvx(const unsigned int itrk, const short ilayer, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionSvx(ilayer, i) : -99999.9);
}

void PHTrackOutv6::set_projectionSvx(const unsigned int itrk, const short ilayer, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionSvx(ilayer, i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_projectionTof(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionTof(i) : -99999.9);
}

void PHTrackOutv6::set_projectionTof(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionTof(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_projectionPbGl(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPbGl(i) : -99999.9);
}

void PHTrackOutv6::set_projectionPbGl(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPbGl(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}
float PHTrackOutv6::get_projectionPbSc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionPbSc(i) : -99999.9);
}

void PHTrackOutv6::set_projectionPbSc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionPbSc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_projectionCrk(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionCrk(i) : -99999.9);
}

void PHTrackOutv6::set_projectionCrk(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionCrk(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}
float PHTrackOutv6::get_projectionAcc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionAcc(i) : -99999.9);
}

void PHTrackOutv6::set_projectionAcc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionAcc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_projectionMrpc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_projectionMrpc(i) : -99999.9);
}

void PHTrackOutv6::set_projectionMrpc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_projectionMrpc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_projectionEmc(const unsigned int itrk, const short i) const
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
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

float PHTrackOutv6::get_directionVtx(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionVtx(i) : -99999.9);
}

void PHTrackOutv6::set_directionVtx(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionVtx(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_directionDch(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionDch(i) : -99999.9);
}

void PHTrackOutv6::set_directionDch(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionDch(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_directionTec(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionTec(i) : -99999.9);
}

void PHTrackOutv6::set_directionTec(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionTec(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_directionPc1(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPc1(i) : -99999.9);
}

void PHTrackOutv6::set_directionPc1(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPc1(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_directionPc2(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPc2(i) : -99999.9);
}

void PHTrackOutv6::set_directionPc2(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPc2(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_directionPc3(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPc3(i) : -99999.9);
}

void PHTrackOutv6::set_directionPc3(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPc3(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_directionSvx(const unsigned int itrk, const short ilayer, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionSvx(ilayer, i) : -99999.9);
}

void PHTrackOutv6::set_directionSvx(const unsigned int itrk, const short ilayer, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionSvx(ilayer, i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_directionTof(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionTof(i) : -99999.9);
}

void PHTrackOutv6::set_directionTof(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionTof(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_directionPbGl(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPbGl(i) : -99999.9);
}

void PHTrackOutv6::set_directionPbGl(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPbGl(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}
float PHTrackOutv6::get_directionPbSc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionPbSc(i) : -99999.9);
}

void PHTrackOutv6::set_directionPbSc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionPbSc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}
float PHTrackOutv6::get_directionAcc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionAcc(i) : -99999.9);
}

void PHTrackOutv6::set_directionAcc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionAcc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_directionMrpc(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionMrpc(i) : -99999.9);
}

void PHTrackOutv6::set_directionMrpc(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionMrpc(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_directionCrk(const unsigned int itrk, const short i) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_directionCrk(i) : -99999.9);
}

void PHTrackOutv6::set_directionCrk(const unsigned int itrk, const short i, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_directionCrk(i,rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}


float PHTrackOutv6::get_emcPathLength(const unsigned int itrk) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_emcPathLength() : -99999.9);
}

void PHTrackOutv6::set_emcPathLength(const unsigned int itrk, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_emcPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_tofPathLength(const unsigned int itrk) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_tofPathLength() : -99999.9);
}

void PHTrackOutv6::set_tofPathLength(const unsigned int itrk, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_tofPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_mrpcPathLength(const unsigned int itrk) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_mrpcPathLength() : -99999.9);
}

void PHTrackOutv6::set_mrpcPathLength(const unsigned int itrk, const float rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_mrpcPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

float PHTrackOutv6::get_crkPathLength(const unsigned int itrk) const
{

  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  return((phtrk) ? phtrk->get_crkPathLength() : -99999.9);
}

void PHTrackOutv6::set_crkPathLength(const unsigned int itrk, const float  rval)
{
  PHSnglTrackv6 *phtrk = (PHSnglTrackv6 *) GetPHTrk()->UncheckedAt(itrk);
  if (phtrk)
    {
      phtrk->set_crkPathLength(rval);
    }
  else
    {
      cerr << PHWHERE << "ERROR no PHSnglTrackv6 object found" << endl;
    }
  return;
}

short PHTrackOutv6::ifIntersectVtx(const unsigned int itrk) const
{
  return((get_projectionVtx(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv6::ifIntersectDch(const unsigned int itrk) const
{
  return((get_projectionDch(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv6::ifIntersectPc1(const unsigned int itrk) const
{
  return((get_projectionPc1(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv6::ifIntersectPc2(const unsigned int itrk) const
{
  return((get_projectionPc2(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv6::ifIntersectPc3(const unsigned int itrk) const
{
  return((get_projectionPc3(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv6::ifIntersectSvx(const unsigned int itrk, const short ilayer) const
{
  return((get_projectionSvx(itrk,ilayer,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv6::ifIntersectCrk(const unsigned int itrk) const
{
  return((get_projectionCrk(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv6::ifIntersectTec(const unsigned int itrk) const
{
  return((get_projectionTec(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv6::ifIntersectTof(const unsigned int itrk) const
{
  return((get_projectionTof(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv6::ifIntersectPbgl(const unsigned int itrk) const
{
  return((get_projectionPbGl(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv6::ifIntersectPbsc(const unsigned int itrk) const
{
  return((get_projectionPbSc(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv6::ifIntersectAcc(const unsigned int itrk) const
{
  return((get_projectionAcc(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv6::ifIntersectMrpc(const unsigned int itrk) const
{
  return((get_projectionMrpc(itrk,1) > -99999. ) ? 1 : 0);
}

short PHTrackOutv6::ifIntersectEmc(const unsigned int itrk) const
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
