#ifdef COMPILE
#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <cstring>
#include <iostream.h>
#include <cmath>
#include <Rtypes.h>
#include <TSystem.h>
#include <TROOT.h>
#include <TFile.h>
#include <TDirectory.h>
#include <TKey.h>
#include <TTree.h>
#include <TBranch.h>
#include <TNtuple.h>
#include <TCanvas.h>
#include <TPad.h>
#include <TStyle.h>
#include <TPostScript.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TGraphErrors.h>
#endif

#include "Pid.hh"

ClassImp(Pid)
//=========================================================================
Pid::Pid() : TObject(){
  reset();
};
//=========================================================================
Pid::Pid(Global& glb,Track& trk,Clust& clt){
  set(glb,trk,clt);
};
//=========================================================================
void Pid::reset(){
  emc_pathl = 0;
  emc_tof = 0;
  signedmom = 0;
  beta = -10;
  invbeta = 0;
  mass = -10;
  mass2 = -10;
  pid = NOPID;
};
//=========================================================================
int Pid::getid(Global& glb,Track& trk,Clust& clt){
  set(glb,trk,clt);
  return pid;
};
//=========================================================================
int Pid::getid(){
  return pid;
};
//=========================================================================
float v_angsectW[4][3] = { { 0.9238, -0.3827, 0 },   // { cos(-22.5deg), sin(-22.5deg), 0 }
			  { 1.0000,  0.0000, 0 },   // { 1, 0, 0 }
			  { 0.9238,  0.3827, 0 },   // { cos(22.5deg), sin(22.5deg), 0 }
			  { 0.3827,  0.9239, 0 } }; // { cos(67.5deg), sin(67.5deg), 0 }
//=========================================================================
void Pid::set(Global& glb,Track& trk,Clust& clt){
  emc_pathl = trk.pathl;
  if( clt.arm == 0 && clt.sector < 4 && clt.sector >= 0 ){
    angle = fabs(trk.dir[0]*v_angsectW[clt.sector][0] +
		 trk.dir[1]*v_angsectW[clt.sector][1] +
		 trk.dir[2]*v_angsectW[clt.sector][2] );
    emc_pathl = trk.pathl - 78.4/angle;
  }
  emc_tof = clt.tofcorr
    + sqrt( clt.pos[0]*clt.pos[0]
	    + clt.pos[1]*clt.pos[1]
	    + (clt.pos[2]-glb.bbcz)*(clt.pos[2]-glb.bbcz))/LIGHTSPEED;
  if( emc_tof < 100 ){
    invbeta = emc_tof*LIGHTSPEED/emc_pathl;
    mass2 = trk.ptot * trk.ptot * (invbeta*invbeta - 1);
    mass = sqrt( fabs( mass2 ) );
  } else {
    invbeta = 0;
    mass2 = 10;
    mass = 10;
  }
  signedmom = -1 * trk.ptot * trk.alpha/fabs(trk.alpha);
  //
  //
  pid = -1;
  epid = -1;
  if( trk.crk_acc > 0 && trk.crk_npmt0 > 2 &&
      trk.crk_disp < 50 && trk.crk_chi2/trk.crk_npe0 < 10 ){
    if( signedmom > 0 )
      epid = ELECTRON;
    else
      epid = POSITRON;
  }
  if( ( epid == ELECTRON || epid == POSITRON ) &&
      (clt.ecore/trk.ptot)>0.8 && (clt.ecore/trk.ptot)<1.2 &&
      clt.ecore>0.3 ){
    if( signedmom > 0 )
      pid = ELECTRON;
    else
      pid = POSITRON;
  }
  else if( mass < 0.3 ){
    if( signedmom > 0 )
      pid = PIONPLUS;
    else
      pid = PIONMINUS;
  }
  else if( mass > 0.5 && mass < 0.6 ){
    if( signedmom > 0 )
      pid = KAONPLUS;
    else
      pid = KAONMINUS;
  }
  else if( mass > 0.8 && mass < 1.2 ){
    if( signedmom > 0 )
      pid = PROTON;
    else
      pid = ANTIPROTON;
  }
  if( clt.tofcorr > 60 )
    pid = -1;
  return;
};
