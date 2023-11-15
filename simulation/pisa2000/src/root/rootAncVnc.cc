// $Id: rootAncVnc.cc,v 1.2 2015/03/19 16:57:01 chiu Exp $

/*!
  \file   rootAncVnc.cc
  \brief  Veto Nosecone Calorimeter PISA evaluation ntuple
  \author Mickey Chiu
  \version $Revision: 1.2 $
  \date    $Date: 2015/03/19 16:57:01 $
*/

#include <iostream>
#include <cstdlib>
#include <fstream>
#include <boost/array.hpp>

#include "PISAEventHeader.h"
#include "PISARun.h"
#include "VncPISAHit.h"

#include <TROOT.h>
#include <TFile.h>
#include <TTree.h>
#include <TBranch.h>
#include <TStopwatch.h>
#include <TDirectory.h>
#include <TRandom.h>
#include <TH1.h>
#include <TH2.h>
#include <TNtuple.h>

using namespace std;

//________________________________________________________________
// if ancflag == 1, close file
void PISARun::AncVnc(const int& ancflag, PISAEvent::pointer pisa_event )
{
  
  // Event header information is not yet used
  int evttrack(0);
  int true_track(0);
  int nfile(0);
  int error(0);
  float ptot(0);
  float ptheta(0);
  float pphi(0);
  float r_vertex(0);
  float z_vertex(0);
  float theta_vertex(0);
  float phi_vertex(0);
  int itparent(0);
  int idparent(0);
  int idpart(0);

  int itorigin(0);
  int idorigin(0);
  float ptotpri(0);
  float pthetpri(0);
  float pphipri(0);
  float z0vertex(0);

  // simulation layer id
  int layerid(0);				
  
  // phi, theta of VNC hit
  float phi(0), theta(0);			
  
  // VNC Hit location
  float xglobal(0), yglobal(0), zglobal(0);	
  
  // VNC TOF at hit
  float tofg(0);				
  
  // VNC energy loss
  float DEDX(0);				
  
  // VNC incoming particle info
  float XE(0),YE(0),Pmom(0),P_id(0),PNum(0);		

  // number of calls
  static int icall = 0;			

  // ntuples data
  static const int NTPL_PARAM = 37;
  boost::array< float, NTPL_PARAM> evt_ntpl;
  evt_ntpl.assign(0);
  
  // Initialization
  static TFile *AncVncFile = 0;		  // output file name
  static TNtuple *AncVncNtuple = 0;	// output Anc Vnc Ntuple

  // ancflag=1 is called at the end, to close the file
  if (ancflag == 1) 
  {

    // Check that the file has been opened
    if(icall == 0 || AncVncFile == 0)
    {
      std::cerr << "\n rootAncVnc <E> bad call with ancflag = 1" << std::endl;
      std::exit(1);
    }  // safety check
    AncVncFile->Write();
    AncVncFile->Close();
    return;
    
  } 

  // Initialize the TNtuple on the first call
  if (icall == 0) 
  {

    // NTUPLE files
    AncVncFile = new TFile("ancmpc.root", "recreate", "Vnc PISA NTUPLE");
    AncVncNtuple = new TNtuple("AncVnc", "Vnc Ancestors",
      "TRACK:NFILE:PTOT:PTHETA:PPHI:"//0
      "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"//5
      "ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"//9
      "LAYER:THETA:PHI:ZGLOBAL:PTOT_PRI:PTHE_PRI:"//14
      "PPHI_PRI:Z0ORIGIN:ETRACK:"//20 
      "XGLOBAL:YGLOBAL:"//23 
      "TOFG:ITORIGIN:"//25
      "IDORIGIN:"//27
      "Z0_EVENT:B_IMPACT:EVENT:"//28
      "DEDX:XE:YE:"//31
      "Pmom:P_id:PNum");//34
  }  // check on initialization

  icall++;

  Int_t mpcRows = VncPISAHit::GetVncCount();
  VncPISAHit *mpcghit = VncPISAHit::GetVncHitEvt();
  
  //cout << "ancmpc: kpart1/2 " << kpart1 << "\t" << kpart2 << endl;
  //cout << "ancmpc: VncCount " << mpcRows << endl;
  
  Int_t iRow;
  for (iRow = 0; iRow < mpcRows ; iRow++) 
  {
    
    true_track = mpcghit[iRow].GetMctrack();
    layerid = mpcghit[iRow].GetLayer();
    
  } 
    
  for (iRow = 0; iRow < mpcRows ; iRow++) 
  {
    
    true_track = mpcghit[iRow].GetMctrack();
    
    // not working?
    dio_TrueTrackToEtrack(&true_track, &evttrack, &nfile);  
    
    dio_ptrkorigin(&true_track, &nfile, &error, 
      &ptotpri, &pthetpri, &pphipri,
      &r_vertex, &z0vertex, &theta_vertex, &phi_vertex, 
      &itorigin, &idorigin, &idpart);
    
    dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		  &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		  &itparent, &idparent, &idpart);

    if(evttrack < 1) {
      if(itorigin > 0) {
	//
	// Something wrong with the true_track to event track (track number in event input file) in some cases??
	// Try again to find the event track number using the origin track number
	//
	dio_TrueTrackToEtrack(&itorigin, &evttrack, &nfile);
	if(evttrack < 1) {
	  //
	  // Still unable to get a valid event track number
	  //
	  std::cerr << "\n rootAncVnc <E> : evttrack invalid " << evttrack << ", itorigin = " << itorigin << " at event " << icall << std::endl;
	} // check on valid evttrack number
	  
      } // check on valid origin track number
      else {
	std::cerr << "\n rootAncVnc <E> : evttrack invalid " << evttrack << ", itorigin = " << itorigin << " at event " << icall << std::endl;
      }
    } // check on valid event track number
    
    layerid = mpcghit[iRow].GetLayer();
    
    evt_ntpl[0] = float(true_track);	// TRACK
    evt_ntpl[1] = float(nfile);	      // NFILE
    evt_ntpl[2] = ptot;		            // PTOT
    evt_ntpl[3] = ptheta;		          // PTHETA
    evt_ntpl[4] = pphi;               // PPHI 
    evt_ntpl[5] = r_vertex;           // R_VERTEX 
    evt_ntpl[6] = z_vertex;           // Z_VERTEX 
    evt_ntpl[7] = theta_vertex;       // THET_VER 
    evt_ntpl[8] = phi_vertex;         // PHI_VER 
    evt_ntpl[9] = float(itparent);    // ITPARENT 
    evt_ntpl[10] = float(idparent);   // IDPARENT 
    evt_ntpl[11] = float(idpart);     // IDPART 
    evt_ntpl[12] = iRow;              // IHIT 
    evt_ntpl[13] = mpcRows;           // NHIT 
    evt_ntpl[14] = layerid;           // IPC 
    xglobal = mpcghit[iRow].GetXin(); 
    yglobal = mpcghit[iRow].GetYin(); 
    zglobal = mpcghit[iRow].GetZin(); 
    tofg = mpcghit[iRow].GetTofg(); 

    float zdiff = zglobal; 
    theta = RAD2DEG*acos(zdiff/sqrt(xglobal*xglobal + yglobal*yglobal + zdiff*zdiff)); 
    phi = RAD2DEG*atan2(yglobal, xglobal); 

    if (phi < -90.0) phi = 360.0 + phi; 

    evt_ntpl[15] = theta;               // THETA 
    evt_ntpl[16] = phi;                 // PHI 
    evt_ntpl[17] = zglobal;             // ZGLOBAL 

    evt_ntpl[18] = ptotpri;             // PTOT_PRI 
    evt_ntpl[19] = pthetpri;            // PTHE_PRI 
    evt_ntpl[20] = pphipri;             // PPHI_PRI 
    evt_ntpl[21] = z0vertex;            // Z0ORIGIN 
    evt_ntpl[22] = evttrack;            // ETRACK 
    evt_ntpl[23] = xglobal;             // XGLOBAL 
    evt_ntpl[24] = yglobal;             // YGLOBAL 
    evt_ntpl[25] = tofg;                // TOFG 
    evt_ntpl[26] = itorigin;            // ITORIGIN 
    evt_ntpl[27] = idorigin;            // IDORIGIN 
    DEDX=mpcghit[iRow].GetDedx(); 
    XE=mpcghit[iRow].GetXe(); 
    YE=mpcghit[iRow].GetYe(); 
    Pmom=mpcghit[iRow].GetPmom(); 
    P_id=mpcghit[iRow].GetP_id(); 
    PNum=mpcghit[iRow].GetPNum(); 
    evt_ntpl[31] = DEDX;              // DEDX 
    evt_ntpl[32] = XE;                // XE 
    evt_ntpl[33] = YE;                // YE 
    evt_ntpl[34] = Pmom;              // Pmom 
    evt_ntpl[35] = P_id;              // P_id 
    evt_ntpl[36] = PNum;              // PNum 
    AncVncNtuple->Fill( &evt_ntpl[0] ); 

  } // loop on rows for this VNCs

  return;
}
