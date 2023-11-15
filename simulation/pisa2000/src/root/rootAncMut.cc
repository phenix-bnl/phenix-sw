// $Id: rootAncMut.cc,v 1.7 2008/02/20 12:03:48 hpereira Exp $

/*!
  \file   rootAncMut.cc
  \brief  Muon Tracker PISA evaluation ntuple
  \author Charlie Maguire
  \version $Revision: 1.7 $
  \date    $Date: 2008/02/20 12:03:48 $
*/

#include <iostream>
#include <cstdlib>
#include <cmath>
#include <boost/array.hpp>

#include "MutPISAHit.h"
#include "PISAEventHeader.h"
#include "PISARun.h"
#include "rootAnc.h"

//___________________________________________________________________
void PISARun::AncMut( const int& ancflag, PISAEvent::pointer pisa_event)
{
  
  static int icall = 0;
  static TFile *AncMutFile = 0;
  static TNtuple *AncMutNtuple = 0;
  
  // retrieve pisa event header
  PISAEventHeader *EventHeader = pisa_event->GetHeader();

  if(ancflag == 1) {

    // Check that the file has been opened
    if(icall == 0 || AncMutFile == 0)
    {
      std::cout << "PISARun::AncMut - bad call with ancflag = 1" << std::endl;
      std::exit(1);
    }

    AncMutFile->Write();
    AncMutFile->Close();
    return;

  } 

  // ntuples data
  static const int NTPL_PARAM = 30;
  boost::array< float, NTPL_PARAM> evt_ntpl;
  evt_ntpl.assign(0);

  // create tree
  if(icall == 0){

    // NTUPLE files
    AncMutFile = new TFile("ancmut.root", "recreate", "MuTr PISA NTUPLE");
    AncMutNtuple = new TNtuple("AncMut", "MuTr Ancestors",
      "TRACK:NFILE:PTOT:PTHETA:PPHI:"//
      "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"//
      "ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"//
      "ISEGMENT:IARM:ISTATION:IPLANE:"// 
      "ITORIGIN:IDORIGIN:THETA:PHI:"//
      "PTOT_PRI:PTHE_PRI:PPHI_PRI:"//
      "PTOT_LOC:PTHE_LOC:PPHI_LOC:"//
      "Z0_EVENT:EVENT" );

  }  // initialization
  icall++;

  int mutRows;
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

  float ptotloc;
  float pthetloc;
  float pphiloc;
  float pxloc;
  float pyloc;
  float pzloc;

  int iposition;
  int isegment;
  int iarm;
  int istation;
  int iplane;

  float xyz[3];
  float theta;
  float phi;
  float rdist;

  evt_ntpl[NTPL_PARAM - 1] = icall;
  evt_ntpl[NTPL_PARAM - 2] = EventHeader->GetZvertex();

  mutRows = MutPISAHit::GetMutCount();
  MutPISAHit *mumhits = MutPISAHit::GetMutHitEvt();

  for (iRow = 0; iRow < mutRows ; iRow++) {
    true_track = mumhits[iRow].GetMctrack();

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
    evt_ntpl[13] = mutRows;

    iposition = mumhits[iRow].GetPlane();;
    isegment = iposition/1000;
    isegment = (iposition - isegment*1000)/100;
    evt_ntpl[14] = isegment;
    iarm = iposition/1000;
    evt_ntpl[15] = iarm;
    istation = iposition/100;
    istation = (iposition - istation*100)/10;
    evt_ntpl[16] = istation;
    iplane = iposition/10;
    iplane = iposition - iplane*10;
    evt_ntpl[17] = iplane;
    evt_ntpl[18] = itorigin;
    evt_ntpl[19] = idorigin;
    xyz[0] = mumhits[iRow].GetX();
    xyz[1] = mumhits[iRow].GetY();
    xyz[2] = mumhits[iRow].GetZ();
    rdist = sqrt(xyz[0]*xyz[0] + xyz[1]*xyz[1]);

    theta = 57.29577951*atan(rdist/xyz[2]);
    if(theta < 0.0)
      theta = 180.0 + theta;

    phi = 57.29577951*atan2(xyz[1], xyz[0]);

    evt_ntpl[20] = theta;
    evt_ntpl[21] = phi;
    evt_ntpl[22] = ptotpri;
    evt_ntpl[23] = pthetpri;
    evt_ntpl[24] = pphipri;

    pxloc = mumhits[iRow].GetPx();
    pyloc = mumhits[iRow].GetPy();
    pzloc = mumhits[iRow].GetPz();

    ptotloc = sqrt(pxloc*pxloc + pyloc*pyloc + pzloc*pzloc);
    if(ptotloc > 0.0) 
    {
      pthetloc = 57.29577951*acos(pzloc/ptotloc);
      pphiloc = 57.29577951*atan2(pyloc, pxloc);
    } else {
      pthetloc = -999.0;
      pphiloc = -999.0;
    }
  
    evt_ntpl[25] = ptotloc;
    evt_ntpl[26] = pthetloc;
    evt_ntpl[27] = pphiloc;

    AncMutNtuple->Fill( &evt_ntpl[0] );  

  }  // Loop over hits in Muon Tracking Stations

  return;
}
