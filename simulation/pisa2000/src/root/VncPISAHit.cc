// $Id: VncPISAHit.cc,v 1.2 2015/03/19 16:57:01 chiu Exp $

/*!
  \file  VncPISAHit.cc
  \brief container for muon piston calorimeter pisa hits
  \author  M. Chiu
  \version $Revision: 1.2 $
  \date    $Date: 2015/03/19 16:57:01 $
*/

#include "VncPISAHit.h"
#include <iostream>

using namespace std;

ClassImp(VncPISAHit)
  
//____________________________________________________________________________
vector<VncPISAHit> VncPISAHit::_hits;
  
//____________________________________________________________________________
VncPISAHit::VncPISAHit(
  Float_t argxx, Float_t argyy, Float_t argzz, 
  Float_t argdedx, Float_t argxe, Float_t argye, 
  Float_t argpmom, Float_t argpid, Float_t argpnum,  
  Int_t argtrack, Int_t argarm, Int_t arglayer, Float_t argtof,
  Int_t argisubevent, Int_t argmctrack, Int_t argnfile)
{
  mctrack = argmctrack;
  xx_vnc = argxx;
  yy_vnc = argyy;
  zz_vnc = argzz; 
  tofg_vnc = argtof;
  dedx_vnc = argdedx;
  Xe_vnc   = argxe;
  Ye_vnc   = argye;
  Pmom_vnc = argpmom;
  P_id_vnc = argpid;
  PNum_vnc  = argpnum; 
  track = argtrack;
  arm = argarm; 
  layer = arglayer;
  isubevent = argisubevent;
  nfile = argnfile;

}

void VncPISAHit::Print(Option_t *opt) const
{
  cout << "VncPisaHit: "<<
   id << "\t" <<		// ! always 0?
   mctrack << "\t" <<		// !same as true_track?
   arm << "\t" <<          // 0 = south, 1 = north
   layer << "\t" <<        // simulation layer id
   xx_vnc << "\t" <<       // x of hit in mpc
   yy_vnc << "\t" <<       // y of hit in mpc
   zz_vnc << "\t" <<       // z of hit in mpc
   tofg_vnc << "\t" <<     // tof of hit in mpc
   dedx_vnc << "\t" <<     // edep in mpc
   Xe_vnc << "\t" <<       // X location of parent on front face of MPC
   Ye_vnc << "\t" <<       // Y location of parent on front face of MPC
   Pmom_vnc << "\t" <<     // Parent Mom in MPC
   P_id_vnc << "\t" <<     // Parent ID
   PNum_vnc << "\t" <<     // particle track number?
   track << "\t" <<        // track in subevent
   isubevent << "\t" <<    // subevent number
   nfile << "\t" << endl;
}

