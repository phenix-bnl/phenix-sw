// $Id: rootAncMui.cc,v 1.8 2009/10/01 22:13:07 hpereira Exp $

/*!
  \file   rootAncMut.cc
  \brief  Muon Identifier PISA evaluation ntuple
  \author Charlie Maguire
  \version $Revision: 1.8 $
  \date    $Date: 2009/10/01 22:13:07 $
*/

#include <iostream>
#include <cstdlib>
#include <cmath>
#include <boost/array.hpp>

#include "MuiPISAHit.h"
#include "PISAEventHeader.h"
#include "PISARun.h"
#include "rootAnc.h"

//___________________________________________________________________
void PISARun::AncMui( const int& ancflag, PISAEvent::pointer pisa_event)
{
  static int icall = 0;
  static TFile *AncMuiFile = 0;
  static TNtuple *AncMuiNtuple = 0;

  // retrieve pisa event header
  PISAEventHeader *EventHeader = pisa_event->GetHeader();

  if(ancflag == 1)
  {

    // Check that the file has been opened
    if(icall == 0 || AncMuiFile == 0){
      std::cout << "PISARun::AncMui - bad call with ancflag = 1" << std::endl;
      std::exit(1);
    }  // safety check
    AncMuiFile->Write();
    AncMuiFile->Close();
    return;
  }

  // ntuples data
  static const int NTPL_PARAM = 29;
  boost::array< float, NTPL_PARAM> evt_ntpl;
  evt_ntpl.assign(0);

  if(icall == 0){

    // NTUPLE files
    AncMuiFile = new TFile("ancmui.root", "recreate", "MuID PISA NTUPLE");
    AncMuiNtuple = new TNtuple("AncMui", "MuID Ancestors",
      "TRACK:NFILE:PTOT:PTHETA:PPHI:R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:ITPARENT:"
      "IDPARENT:IDPART:IHIT:NHIT:IARM:IPLANE:ITORIGIN:IDORIGIN:THETA:PHI:"
      "PTOT_PRI:PTHE_PRI:PPHI_PRI:XHIT:YHIT:ZHIT:"
      "PISAEVENT:Z0_EVENT:EVENT");

  }  // initialization
  icall++;

  int muiRows;
  int iRow;

  int true_track;
  int nfile;
  int error;
  float ptot;
  float ptheta;
  float pphi;
  float r_vertex;
  float z_vertex;
  float theta_vertex;
  float phi_vertex;
  int itparent;
  int idparent;
  int idpart;

  int itorigin;
  int idorigin;
  float ptotpri;
  float pthetpri;
  float pphipri;
  float z0vertex;

  int iposition;
  int iarm;
  int iplane;

  float xyz[3];
  float theta;
  float phi;
  float rdist;

  muiRows = MuiPISAHit::GetMuiCount();
  MuiPISAHit *munhits = MuiPISAHit::GetMuiHitEvt();

  for (iRow = 0; iRow < muiRows ; iRow++)
  {
    true_track = munhits[iRow].GetMctrack();

    dio_ptrkorigin(&true_track, &nfile, &error,
                   &ptotpri, &pthetpri, &pphipri,
	  	   &r_vertex, &z0vertex, &theta_vertex, &phi_vertex,
		   &itorigin, &idorigin, &idpart);

    /*
    *  NOTE: idpart is primary particle id (=idorigin), not the input particle id.
    */

    dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		  &r_vertex, &z_vertex, &theta_vertex, &phi_vertex,
		  &itparent, &idparent, &idpart);

    evt_ntpl[0] = float(true_track);
    evt_ntpl[1] = float(nfile);
    evt_ntpl[2] = ptot;
    evt_ntpl[3] = ptheta;
    evt_ntpl[4] = pphi;
    evt_ntpl[5] = r_vertex;
    evt_ntpl[6] = z_vertex;
    evt_ntpl[7] = theta_vertex;
    evt_ntpl[8] = phi_vertex;
    evt_ntpl[9] = float(itparent);
    evt_ntpl[10] = float(idparent);
    evt_ntpl[11] = float(idpart);
    evt_ntpl[12] = iRow + 1;   // counts from 1 here
    evt_ntpl[13] = muiRows;

    iposition = munhits[iRow].GetPlane_num();
    iarm = iposition/1000;
    iplane = (iposition - iarm*1000);
    evt_ntpl[14] = iarm;
    evt_ntpl[15] = iplane;
    evt_ntpl[16] = itorigin;
    evt_ntpl[17] = idorigin;
    xyz[0] = munhits[iRow].GetRhit1();
    xyz[1] = munhits[iRow].GetRhit2();
    xyz[2] = munhits[iRow].GetRhit3();
    rdist = sqrt(xyz[0]*xyz[0] + xyz[1]*xyz[1]);

    theta = 57.29577951*atan(rdist/xyz[2]);
    if(theta < 0.0)
      theta = 180.0 + theta;

    phi = 57.29577951*atan2(xyz[1], xyz[0]);

    evt_ntpl[18] = theta;
    evt_ntpl[19] = phi;
    evt_ntpl[20] = ptotpri;
    evt_ntpl[21] = pthetpri;
    evt_ntpl[22] = pphipri;
    evt_ntpl[23] = xyz[0];
    evt_ntpl[24] = xyz[1];
    evt_ntpl[25] = xyz[2];

    evt_ntpl[26] = EventHeader->GetEvent();
    evt_ntpl[27] = EventHeader->GetZvertex();
    evt_ntpl[28] = icall;

    AncMuiNtuple->Fill(&evt_ntpl[0]);

  } // loop over rows

  return;
}
