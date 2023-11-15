#include <iostream>
#include <cstdlib>

#include "root_ptrk.h"

//_______________________________________________________________________________________
extern "C" int dio_TrueTrackToEtrack (int *true_track, int *etrack, int *nfile)
{


  // Associate True Track with a Primary Event track
  *etrack = -4;  // default
  *nfile = -4;   // default

  KinPISAHit *KinHitEvt = KinPISAHit::GetKinHitEvt();
  Int_t *KinTrkIndex = KinPISAHit::GetKinTrkIndex();
  if(*true_track > 0) {
    int kin = KinTrkIndex[*true_track -1];
    if(kin > -1) {
      *etrack = KinHitEvt[kin].GetEvttrack();
      *nfile =  KinHitEvt[kin].GetNfile();
    }
  }

  return 0;
}

//_______________________________________________________________________________________
extern "C" int dio_Etrackstack (int *etrack, int *nfile, int *error, float *px,	float *py, float *pz,  int *idpart)
{
  Int_t PriCount = PriPISAHit::GetPriCount();
  if(*etrack < 1 || *etrack > PriCount) {
    std::cerr << "\n dio_Etrackstack <E>: invalid etrack value " << *etrack << std::endl;
    std::exit(1);
  }

  //
  // Retrieve the etrack kinematics information
  //
  PriPISAHit *PriHitEvt = PriPISAHit::GetPriHitEvt();
  PriPISAHit *ptr = &PriHitEvt[*etrack - 1];
  *nfile = ptr->GetNfile();
  *px = ptr->GetPx();
  *py = ptr->GetPy();
  *pz = ptr->GetPz();
  *idpart = ptr->GetIdpart();
  *error = 0;

  return 0;
}

//_______________________________________________________________________________________
extern "C" int dio_ptrkstack(
  int *true_track, int *nfile,int *error, 
  float *ptot, float *ptheta, float *pphi,
  float *r_vertex, float *z_vertex, 
  float *theta_vertex, float *phi_vertex,
  int *itparent, int *idparent, int *idpart)
{

  // std::cout << "do_ioptrkstack - this method is obsolete. It is recommanded to use KinPISAHit::Find( true_track ) instead." << std::endl;
  
  assert( *true_track - 1 < KinPISAHit::GetKinMaxTrack() );
  const KinPISAHit& hit( *KinPISAHit::Find( *true_track ) );
  
  *error = 0;
  *nfile = hit.GetNfile();
  *ptot = hit.GetPtot();
  *ptheta = hit.GetPthet();
  *pphi = hit.GetPhi();
  *r_vertex = hit.GetRvertex();
  *z_vertex = hit.GetZvertex();
  *theta_vertex = hit.GetThvertx();
  *phi_vertex = hit.GetPhvertx();
  *itparent = hit.GetItparent();
  *idparent = hit.GetIdparent();
  *idpart = hit.GetIdpart();
  return 0;

}

//_______________________________________________________________________________________
extern "C" int dio_ptrkorigin(
  int *true_track, int *nfile, int *error,
  float *ptot, float *ptheta, float *pphi,
  float *r_vertex, float *z_vertex,
  float *theta_vertex, float *phi_vertex,
  int *itorigin, int *idorigin, int *idpart)
{

  // std::cout << "dio_ptrkorigin - this method is obsolete. It is recommanded to use KinPISAHit::Find( true_track ) instead." << std::endl;
  
  assert( *true_track - 1 < KinPISAHit::GetKinMaxTrack() );
  const KinPISAHit& hit( *KinPISAHit::FindOrigin( *true_track ) );
  
  *error = 0;
  *nfile = hit.GetNfile();
  *ptot = hit.GetPtot();
  *ptheta = hit.GetPthet();
  *pphi = hit.GetPhi();
  *r_vertex = hit.GetRvertex();
  *z_vertex = hit.GetZvertex();
  *theta_vertex = hit.GetThvertx();
  *phi_vertex = hit.GetPhvertx();
  *itorigin = hit.GetItparent();
  *idorigin = hit.GetIdpart();
  *idpart = hit.GetIdpart();
  return 0;

}
