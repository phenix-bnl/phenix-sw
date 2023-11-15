// $Id: MpcPISAHit.cc,v 1.8 2011/02/14 05:27:50 bbannier Exp $

/*!
  \file  MpcPISAHit.h
  \brief container for muon piston calorimeter pisa hits
  \author  M. Chiu
  \version $Revision: 1.8 $
  \date    $Date: 2011/02/14 05:27:50 $
*/

#include "MpcPISAHit.h"
#include <iostream>

using namespace std;

ClassImp(MpcPISAHit)
  
//____________________________________________________________________________
vector<MpcPISAHit> MpcPISAHit::_hits;
  
//____________________________________________________________________________
MpcPISAHit::MpcPISAHit(
  Float_t argxx, Float_t argyy, Float_t argzz, 
  Float_t argdedx, Float_t argxe, Float_t argye, 
  Float_t argpmom, Float_t argpid, Float_t argpnum,  
  Int_t argtrack, Int_t argarm, Int_t argtow, Float_t argtof,
  Int_t argisubevent, Int_t argmctrack, Int_t argnfile)
{
  mctrack = argmctrack;
  xx_mpc = argxx;
  yy_mpc = argyy;
  zz_mpc = argzz; 
  tofg_mpc = argtof;
  dedx_mpc = argdedx;
  Xe_mpc   = argxe;
  Ye_mpc   = argye;
  Pmom_mpc = argpmom;
  P_id_mpc = argpid;
  PNum_mpc  = argpnum; 
  track = argtrack;
  arm = argarm; 
  tower = argtow;
  isubevent = argisubevent;
  nfile = argnfile;

}

void MpcPISAHit::Print(Option_t *opt) const
{
  cout << "MpcPisaHit: "<<
   id << "\t" <<		// ! always 0?
   mctrack << "\t" <<		// !same as true_track?
   xx_mpc << "\t" <<       // x of hit in mpc
   yy_mpc << "\t" <<       // y of hit in mpc
   zz_mpc << "\t" <<       // z of hit in mpc
   tofg_mpc << "\t" <<     // tof of hit in mpc
   dedx_mpc << "\t" <<     // edep in mpc
   Xe_mpc << "\t" <<       // X location of parent on front face of MPC
   Ye_mpc << "\t" <<       // Y location of parent on front face of MPC
   Pmom_mpc << "\t" <<     // Parent Mom in MPC
   P_id_mpc << "\t" <<     // Parent ID
   PNum_mpc << "\t" <<     // particle track number?
   track << "\t" <<        // track in subevent
   arm << "\t" <<          // 0 = south, 1 = north
   tower << "\t" <<        // simulation tower id
   isubevent << "\t" <<    // subevent number
   nfile << "\t" << endl;
}

