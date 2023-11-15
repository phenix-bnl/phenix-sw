#include <iostream>
#include <cstdlib>
#include "PISARun.h"
#include "PISAEventHeader.h"
#include "RxnPISAHit.h"
#include <fstream>

#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TStopwatch.h"
#include "TDirectory.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"

using namespace std;

//_________________________________________________________________
void PISARun::AncRxn( const int& ancflag, PISAEvent::pointer pisa_event)
{
 
  // retrieve pisa event header
  PISAEventHeader *EventHeader = pisa_event->GetHeader();

  // Event header information not yet used
  //const int maxpart = 10000;
  //int pctrack[maxpart];
  //int kpart;

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

  float theta(0);
  float phi(0);
  float xglobal, yglobal, zglobal;
  const float DEGRAD = 57.295779513;

  static int icall = 0;
  const int NTPL_PARAM = 43;
  float evt_ntpl[NTPL_PARAM];

  //
  // Initialization
  //
  static TFile *AncRxnFile = 0;
  static TNtuple *AncRxnNtuple = 0;

  if(ancflag == 1) {
    //
    // Check that the file has been opened
    //
    if(icall == 0 || AncRxnFile == 0){
      cout << "PISARun::AncRxn - bad call with ancflag = 1" << endl;
      exit(1);
    }  // safety check
    AncRxnFile->Write();
    AncRxnFile->Close();
    return;
  }  // check for close output file flag

  if(icall == 0){
    //
    // NTUPLE files
    //
    AncRxnFile = new TFile("ancrxn.root", "recreate", "RXN PISA NTUPLE");
    AncRxnNtuple = new TNtuple("AncRxn", "RXN Ancestors",
      "TRACK:NFILE:PTOT:PTHETA:PPHI:"//   4
      "R_VERTEX:Z_VERTEX:THET_VER:PHI_VER:"// 8
      "ITPARENT:IDPARENT:IDPART:IHIT:NHIT:"// 13
      "THETA:PHI:DELE:PTOT_PRI:PTHE_PRI:"// 18
      "PPHI_PRI:Z0ORIGIN:ETRACK:"// 21 
      "XGLOBAL:XLOCAL1:XLOCAL2:"// 24
      "YGLOBAL:YLOCAL1:YLOCAL2:"// 27
      "ZGLOBAL:ZLOCAL1:ZLOCAL2:"// 30
      "ITORIGIN:IDORIGIN:ARM:"// 33
      "TOFRXN:PATHRXN:BETARXN:"// 36
      "PMOMX:PMOMY:PMOMZ:"// 39
      "Z0_EVENT:B_IMPACT:EVENT"); // 42
  }  // check on initialization

  icall++;

  int rxnRows;
  int iRow;

  evt_ntpl[NTPL_PARAM - 1] = icall;
  rxnRows = RxnPISAHit::GetRxnCount();
  RxnPISAHit *rxnghit = RxnPISAHit::GetRxnHitEvt();

  evt_ntpl[NTPL_PARAM - 1] = icall; // used as eventnumber
  evt_ntpl[NTPL_PARAM - 2] = EventHeader->GetImpactParameter();
  evt_ntpl[NTPL_PARAM - 3] = EventHeader->GetZvertex();  // z vertex of event

  evt_ntpl[13] = rxnRows; // totaal number of hits in this event for TOF-West
  for (iRow = 0; iRow < rxnRows ; iRow++) {

    true_track = rxnghit[iRow].GetMctrack();
    if(true_track < 1)
      continue ;  // this is an error condition

    dio_TrueTrackToEtrack(&true_track, &evttrack, &nfile);

    //
    // the original primary particle
    //
    dio_ptrkorigin(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
      &r_vertex, &z0vertex, &theta_vertex, &phi_vertex, 
      &itorigin, &idorigin, &idpart);  
/*
*  NOTE: idpart is primary particle id (=idorigin), not the input particle id.
*/
      
    //
    // the immediate ancestor of this particle
    //
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
	  std::cerr << "\n rootAncRxn <E> : evttrack invalid " << evttrack << ", itorigin = " << itorigin << " at event " << icall << std::endl;
	} // check on valid evttrack number
	  
      } // check on valid origin track number
      else {
	std::cerr << "\n rootAncRxn <E> : evttrack invalid " << evttrack << ", itorigin = " << itorigin << " at event " << icall << std::endl;
      }
    } // check on valid event track number

    evt_ntpl[0] = float(true_track);
    evt_ntpl[1] = float(nfile);  // used for multiple (MERGE) file inputs
    evt_ntpl[2] = ptot;  // total momentum of this particle
    evt_ntpl[3] = ptheta; // polar direction of this particle's momentum
    evt_ntpl[4] = pphi;   // azimuthal direction of this particle's momentum
    evt_ntpl[5] = r_vertex;  // radial position of this particle's original vertex
    evt_ntpl[6] = z_vertex;  // z position of this particle's original vertex
    evt_ntpl[7] = theta_vertex; // polar angle of this particle's original vertex
    evt_ntpl[8] = phi_vertex;   // azimuthal angle of this particle's original vertex
    evt_ntpl[9] = float(itparent);  // track number of the parent of this particle
    evt_ntpl[10] = float(idparent); // GEANT particle ID of the parent of this particle
    evt_ntpl[11] = float(idpart);   // GEANT particle of ID of this particle
    evt_ntpl[12] = iRow+1; // hit number in TOF-West for this event

    xglobal = rxnghit[iRow].GetXing(); // global (lab frame) x position at entrance to TOF-West
    yglobal = rxnghit[iRow].GetYing(); // global y position at entrance to TOF-West
    zglobal = rxnghit[iRow].GetZing(); // global z position at entrance to TOF-West

    float mcrxndis = sqrt(xglobal*xglobal + yglobal*yglobal + zglobal*zglobal);
    theta = DEGRAD*acos(zglobal/mcrxndis);
    phi = DEGRAD*atan2(yglobal, xglobal);
    if( phi < -90.0 )
      phi = 360.0 + phi;

    evt_ntpl[14] = theta; // lab frame polar angle at entrance to TOF-West
    evt_ntpl[15] = phi;   // lab frame azimuthal angle at entrance to TOF-West

    evt_ntpl[16] = rxnghit[iRow].GetDedx();  // energy loss in passing through TOF-West

    evt_ntpl[17] = ptotpri; // total momentum of primary particle origin of this particle
    evt_ntpl[18] = pthetpri; // polar angle direction of the momentum of primary particle origin of this particle
    evt_ntpl[19] = pphipri;  // azimuthal angle direction of the momentum of primary particle origin of this particle
    evt_ntpl[20] = z0vertex; // z0 vertex of this primary particle (should be the same as the event z0)
    evt_ntpl[21] = evttrack; // track number in input event file

    evt_ntpl[22] = xglobal;
    evt_ntpl[23] = rxnghit[iRow].GetXin();  // local (detector frame) x position at entrance to TOF-West
    evt_ntpl[24] = rxnghit[iRow].GetXout(); // local (detector frame) x position at exit from TOF-West
    evt_ntpl[25] = yglobal;
    evt_ntpl[26] = rxnghit[iRow].GetYin();  // local (detector frame) y position at entrance to TOF-West
    evt_ntpl[27] = rxnghit[iRow].GetYout(); // local (detector frame) y position at exit from TOF-West
    evt_ntpl[28] = zglobal;
    evt_ntpl[29] = rxnghit[iRow].GetZin();  // local (detector frame) z position at entrance to TOF-West
    evt_ntpl[30] = rxnghit[iRow].GetZout(); // local (detector frame) z position at exit from TOF-West

    evt_ntpl[31] = itorigin;
    evt_ntpl[32] = idorigin;

    evt_ntpl[33] = rxnghit[iRow].GetArm();;

    evt_ntpl[34] = rxnghit[iRow].GetTof();  // time of flight in ns to entracnce of TOF-West
    evt_ntpl[35] = rxnghit[iRow].GetPathLength(); // path length in cm to entrance of TOF-West
    //
    // compute Beta (= v/c) for this particle if the TOF is greater than 0
    //
    if(evt_ntpl[34]>0.0)
         evt_ntpl[36] = evt_ntpl[35]/(evt_ntpl[34]*29.9792458);
      else
         evt_ntpl[36] = -9999.0;

    evt_ntpl[37] = rxnghit[iRow].GetPx();
    evt_ntpl[38] = rxnghit[iRow].GetPy();
    evt_ntpl[39] = rxnghit[iRow].GetPz();

    AncRxnNtuple->Fill(evt_ntpl);

  } // loop over stored hits

  return;
}

