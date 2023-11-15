// $Id: rootAncRLT.cc,v 1.6 2017/04/25 22:21:36 phnxbld Exp $

/*!
  \file   rootAncRLT.cc
  \brief  RLT PISA evaluation ntuple
  \author Charlie Maguire
  \version $Revision: 1.6 $
  \date    $Date: 2017/04/25 22:21:36 $
*/

#include <iostream>
#include <cstdlib>
#include <fstream>

#include <TFile.h>
#include <TBranch.h>
#include <TNtuple.h>

#include "PISARun.h"
#include "PISAEventHeader.h"
#include "RLTPISAHit.h"

using namespace std;

//________________________________________________________________________
void PISARun::AncRLT( const int& ancflag, PISAEvent::pointer pisa_event)
{
  
  // retrieve event header
  PISAEventHeader *EventHeader = pisa_event->GetHeader();
  
  // Event header information not yet used
  const int maxpart = 10000;
  int ipc123( 0 ); 

  int evttrack( 0 );
  int true_track( 0 );
  int nfile( 0 );
  int error( 0 );
  float ptot( 0 );
  float ptheta( 0 );
  float pphi( 0 );
  float r_vertex( 0 );
  float z_vertex( 0 );
  float theta_vertex( 0 );
  float phi_vertex( 0 );
  int itparent( 0 );
  int idparent( 0 );
  int idpart( 0 );

  float delth13( 0 );
  float delph13( 0 );

  int   trackmin( 0 );
  int   idmin( 0 );
  int   torigmin( 0 );
  float delmin( 0 );
  float realmin( 0 );
  
  int itorigin( 0 );
  int idorigin( 0 );
  float ptotpri( 0 );
  float pthetpri( 0 );
  float pphipri( 0 );
  float z0vertex( 0 );

  int iRPC( 0 );
  float theta( 0 );
  float phi( 0 );
  float deltheta( 0 );
  float z0proj( 0 );
  float xglobal( 0 ), yglobal( 0 ), zglobal( 0 );
  const float DEGRAD = 57.295779513;

  static int icall = 0;
  const int NTPL_PARAM = 50;
  float evt_ntpl[NTPL_PARAM];
  
  static TFile *AncRLTFile = 0;
  static TNtuple *AncRLTNtuple = 0;
  
  if ( ancflag == 1){

    // Check that the file has been opened
    if ( icall == 0 || AncRLTFile == 0 ){
      cout << "\n rootAncRLT <E> bad call with ancflag = 1" << endl;
      exit(1);
    }
    AncRLTFile->Write();
    AncRLTFile->Close();
    return;
    
  }

  if (icall == 0)
  {
    
    //NTUPLe files
    AncRLTFile = new TFile("ancrlt.root", "recreate" , "RLT PISA NTUPLE");
    AncRLTNtuple = new TNtuple(
      "AncRLT", "RLT Ancestors", 
      "TRACK:NFILE:PTOT:PTHETA:PPHI:"//
      "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"//
      "ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"//
      "IRPC:THETA:PHI:ZGLOBAL:PTOT_PRI:PTHE_PRI:"//
      "PPHI_PRI:Z0ORIGIN:ETRACK:ZLOCAL1:ZLOCAL2:"// 
      "XGLOBAL:XLOCAL1:XLOCAL2:"// 
      "YGLOBAL:YLOCAL1:YLOCAL2:IPC123:ITORIGIN:"//
      "IDORIGIN:SECTOR:DELTH13:DELPH13:"//
      "REALMIN:DELMIN:IDMIN:TORIGMIN:TRACKMIN:"//
      "Z0PROJ:DELTHETA:TOFPC:PATHPC:BETAPC:"//
      "Z0_EVENT:B_IMPACT:EVENT");
    
  }
  icall ++;
  Int_t rltRows = RLTPISAHit::GetrltCount();
  Int_t rpcRow[3];
  rpcRow[0] = RLTPISAHit::GetrltRPC1Count();
  rpcRow[1] = RLTPISAHit::GetrltRPC2Count();
  rpcRow[2] = RLTPISAHit::GetrltRPC3Count();

  RLTPISAHit *rltrpc1ghit = RLTPISAHit::GetrltRPC1HitEvt();
  RLTPISAHit *rltrpc2ghit = RLTPISAHit::GetrltRPC2HitEvt();
  RLTPISAHit *rltrpc3ghit = RLTPISAHit::GetrltRPC3HitEvt();

  //Test to see if the number of hits in each RPC is less then the maximum??

  for ( iRPC=0; iRPC < 3; iRPC++)
  {
    if ( rpcRow[iRPC] > maxpart )
    {
      cout << "\n rootAncRLT <E> for RPC" << iRPC+1;
      cout << "there are too many hits" << rpcRow[iRPC] << endl;
    }    
  }

  Int_t iRow;
  RLTPISAHit *rltghit = RLTPISAHit::GetrltHitEvt();
  for ( iRow = 0; iRow < rltRows; iRow++)
  {
    
    true_track = rltghit[iRow].GetMctrack();
    iRPC = rltghit[iRow].GetIrpc();
    switch (iRPC) 
    {
      
      case 1:{break;}
      case 2:{break;}
      case 3:{break;}
      default:
      cout << "rootAncRLT <E> invalid RPCx number - " << iRPC << endl; 
      exit(1);
      
    }
  }

  evt_ntpl[NTPL_PARAM - 1] = icall;
  evt_ntpl[NTPL_PARAM - 2] = EventHeader->GetImpactParameter();
  evt_ntpl[NTPL_PARAM - 3] = EventHeader->GetZvertex();

  for(iRPC = 1; iRPC < 4; iRPC++){
    switch (iRPC){
    case 1: {rltghit = rltrpc1ghit; rltRows = rpcRow[iRPC -1]; break;} 
    case 2: {rltghit = rltrpc2ghit; rltRows = rpcRow[iRPC -1]; break;} 
    case 3: {rltghit = rltrpc3ghit; rltRows = rpcRow[iRPC -1]; break;} 
    default: { cout << "\n rootAncRLT <E> Uknown RPCx number" << endl;}
    }
    for(iRow = 0; iRow < rltRows; iRow++){
      true_track = rltghit[iRow].GetMctrack();
      delth13 = -999;
      delph13 = -999;

      dio_TrueTrackToEtrack(&true_track, &evttrack, &nfile);  // not working?
      
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
	    std::cerr << "\n rootAncRLT <E> : evttrack invalid " << evttrack << ", itorigin = " << itorigin << " at event " << icall << std::endl;
	  } // check on valid evttrack number
	  
	} // check on valid origin track number
	else {
	  std::cerr << "\n rootAncRLT <E> : evttrack invalid " << evttrack << ", itorigin = " << itorigin << " at event " << icall << std::endl;
	}
      } // check on valid event track number

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
      evt_ntpl[12] = iRow;
      evt_ntpl[13] = rltRows;
      evt_ntpl[14] = iRPC;
      xglobal = rltghit[iRow].GetXing();
      yglobal = rltghit[iRow].GetYing();
      zglobal = rltghit[iRow].GetZing();

      float zdiff = zglobal;
      theta = DEGRAD*acos(zdiff/sqrt(xglobal*xglobal +
                          yglobal*yglobal + zdiff*zdiff));
      phi = DEGRAD*atan2(yglobal, xglobal);

      if(phi < -90.0)
        phi = 360.0 + phi;

      evt_ntpl[15] = theta;
      evt_ntpl[16] = phi;
      evt_ntpl[17] = zglobal;

      evt_ntpl[18] = ptotpri;
      evt_ntpl[19] = pthetpri;
      evt_ntpl[20] = pphipri;
      evt_ntpl[21] = z0vertex;
      evt_ntpl[22] = evttrack;
      evt_ntpl[23] = rltghit[iRow].GetZin();
      evt_ntpl[24] = rltghit[iRow].GetZout();
      evt_ntpl[25] = xglobal;
      evt_ntpl[26] = rltghit[iRow].GetXin();
      evt_ntpl[27] = rltghit[iRow].GetXout();
      evt_ntpl[28] = yglobal;
      evt_ntpl[29] = rltghit[iRow].GetYin();
      evt_ntpl[30] = rltghit[iRow].GetYout();
      evt_ntpl[31] = ipc123;
      evt_ntpl[32] = itorigin;
      evt_ntpl[33] = idorigin;

      //... not used variables 
      evt_ntpl[35] = delth13;
      evt_ntpl[36] = delph13;
      evt_ntpl[37] = realmin;
      evt_ntpl[38] = delmin;
      evt_ntpl[39] = idmin;
      evt_ntpl[40] = torigmin;
      evt_ntpl[41] = trackmin;
      evt_ntpl[42] = z0proj;
      evt_ntpl[43] = deltheta;

      evt_ntpl[44] =  rltghit[iRow].GetTof(); 
      evt_ntpl[45] =  rltghit[iRow].GetPathLength(); 
      if(evt_ntpl[44]>0.0)
         evt_ntpl[46] = evt_ntpl[45]/(evt_ntpl[44]*29.9792458);
      else
         evt_ntpl[46] = -9999.0;

      AncRLTNtuple->Fill(evt_ntpl);

    } // loop on rows for this RPCx

  } // loop on all 3 RPCs

  return;
}

